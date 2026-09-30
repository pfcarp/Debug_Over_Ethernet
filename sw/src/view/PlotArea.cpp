#include "PlotArea.hpp"


#include <cmath>
#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <iostream>
#include <vector>
#include <epoxy/gl.h>


#include "Description.hpp"
#include "TimemarkerDialog.hpp"
#include "TimemarkerCollection.hpp"


const char* xlabel = "Time (CC)";

// --- Shader Sources ---
static const char* vertex_shader_source = R"glsl(
  #version 300 es
  layout(location = 0) in vec2 aPos;
  layout(location = 1) in vec4 aColor;
  out vec4 vColor;
  uniform mat4 uViewProjection;
  uniform float uZoomScale;
  void main() {
    gl_Position = uViewProjection * vec4(aPos, 0.0, 1.0);
    vColor = aColor;
    gl_PointSize = 10.0; // * uZoomScale;
  }
)glsl";

static const char* fragment_shader_source = R"glsl(
  #version 300 es
  precision highp float; // Required for OpenGL ES
  in vec4 vColor;
  out vec4 FragColor;
  uniform float uRadius;
  void main() {
    vec2 coord = gl_PointCoord - vec2(0.5);
    if (length(coord) > 0.5) discard;
    if (length(coord) > uRadius) discard;
    FragColor = vColor;
  }
)glsl";

// --- Helper: Compile Shaders ---
static GLuint compileShader(GLenum type, const char* source) {
    GLuint shader = glCreateShader(type);
    glShaderSource(shader, 1, &source, NULL);
    glCompileShader(shader);
    GLint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        GLchar infoLog[512];
        glGetShaderInfoLog(shader, 512, NULL, infoLog);
        g_warning("Shader compilation failed: %s", infoLog);
        return 0;
    }
    return shader;
}

// --- Helper: Load Shaders ---
static GLuint loadShaders() {
    GLuint vertex_shader = compileShader(GL_VERTEX_SHADER, vertex_shader_source);
    GLuint fragment_shader = compileShader(GL_FRAGMENT_SHADER, fragment_shader_source);
    if (!vertex_shader || !fragment_shader) return 0;

    GLuint program = glCreateProgram();
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);
    GLint success;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (!success) {
        GLchar infoLog[512];
        glGetProgramInfoLog(program, 512, NULL, infoLog);
        g_warning("Shader linking failed: %s", infoLog);
        return 0;
    }
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    return program;
}

// -----------
PlotArea::PlotArea(uint32_t id, PlotAreaTracker& tracker): id(id), tracker(tracker) {
  tracker.link(this);
  parent = gtk_gl_area_new();
  gtk_widget_set_hexpand(parent, TRUE);
  gtk_widget_set_vexpand(parent, TRUE);

  // Connect OpenGL signals
  g_signal_connect(parent, "realize", G_CALLBACK(cOnRealize), this);
  g_signal_connect(parent, "render", G_CALLBACK(cOnRender), this);
  g_signal_connect(parent, "unrealize", G_CALLBACK(cOnUnrealize), this);

  // Keep existing event controllers
  GtkEventController* scroll = gtk_event_controller_scroll_new(GTK_EVENT_CONTROLLER_SCROLL_VERTICAL);
  g_signal_connect(scroll, "scroll", G_CALLBACK(PlotArea::cOnScroll), this);
  gtk_widget_add_controller(parent, scroll);

  GtkEventController* motion = gtk_event_controller_motion_new();
  g_signal_connect(motion, "motion", G_CALLBACK(cOnMotion), this);
  gtk_widget_add_controller(parent, motion);

  GtkGestureClick* click = GTK_GESTURE_CLICK(gtk_gesture_click_new());
  gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click), 0);
  gtk_widget_add_controller(parent, GTK_EVENT_CONTROLLER(click));
  g_signal_connect(click, "pressed", G_CALLBACK(cOnButtonPress), this);
  g_signal_connect(click, "released", G_CALLBACK(cOnButtonRelease), this);
}


void PlotArea::update() {
  gtk_widget_queue_draw(parent);
}


// --- OpenGL Callbacks ---
void PlotArea::cOnRealize(GtkGLArea* area, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onRealize();
}

void PlotArea::onRealize() {
  gtk_gl_area_make_current(GTK_GL_AREA(parent));
    if (gtk_gl_area_get_error(GTK_GL_AREA(parent)) != NULL) {
        g_warning("Failed to initialize OpenGL");
        return;
    }

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Load shaders (see below)
    shader_program = loadShaders();
    if (!shader_program) {
        g_warning("Failed to load shaders");
        return;
    }

    // VAO/VBO for points (scatter)
    glGenVertexArrays(1, &vao_points);
    glGenBuffers(1, &vbo_points);
    glBindVertexArray(vao_points);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_points);
    glBufferData(GL_ARRAY_BUFFER, 0, NULL, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(2 * sizeof(float)));
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    // VAO/VBO for lines (timemarkers/axes/curves)
    glGenVertexArrays(1, &vao_lines);
    glGenBuffers(1, &vbo_lines);
    glBindVertexArray(vao_lines);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_lines);
    glBufferData(GL_ARRAY_BUFFER, 0, NULL, GL_DYNAMIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(2 * sizeof(float)));
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void PlotArea::cOnRender(GtkGLArea* area, GdkGLContext* context, gpointer user_data) {
    PlotArea* self = static_cast<PlotArea*>(user_data);
    int width = gtk_widget_get_allocated_width(GTK_WIDGET(area));
    int height = gtk_widget_get_allocated_height(GTK_WIDGET(area));
    self->onRender(width, height);
}

void PlotArea::onRender(int width, int height) {
  tracker.plot.width = width;
  tracker.plot.height = height;

  glViewport(0, 0, width, height);
  glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  TraceDatabase& database = TraceDatabase::instance();
  double x_min = database.minTimestamp();
  double x_max = database.maxTimestamp();
  double y_min = database.minCount();
  double y_max = database.maxCount();

  double x_range = x_max - x_min;
  double y_range = y_max - y_min;
  double x_center = (x_min + x_max) / 2.0;
  double y_center = (y_min + y_max) / 2.0;

  double s = tracker.viewport.scale;
  double xo = tracker.viewport.offset.x;
  double yo = tracker.viewport.offset.y;

  // Corrected view projection matrix that centers data properly
  float view_projection[16] = {
    static_cast<float>(2.0 * s / x_range),  0.0f, 0.0f, 0.0f,
    0.0f, static_cast<float>(2.0 * s / y_range), 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    static_cast<float>(-2.0 * s * x_center / x_range + xo),
    static_cast<float>(-2.0 * s * y_center / y_range + yo),
    0.0f, 1.0f
  };

  glUseProgram(shader_program);
  glUniformMatrix4fv(glGetUniformLocation(shader_program, "uViewProjection"), 1, GL_FALSE, view_projection);
  glUniform1f(glGetUniformLocation(shader_program, "uZoomScale"), static_cast<float>(s));

  // Draw everything
  const auto& variants = TraceDatabase::instance()[id].getVariants();
  for (const std::string& variant : variants) {
    plotScatterGL(variant, width, height);
  }
}

void PlotArea::cOnUnrealize(GtkGLArea* area, gpointer user_data) {
    PlotArea* self = static_cast<PlotArea*>(user_data);
    self->onUnrealize();
}

void PlotArea::onUnrealize() {
    glDeleteVertexArrays(1, &vao_points);
    glDeleteBuffers(1, &vbo_points);
    glDeleteVertexArrays(1, &vao_lines);
    glDeleteBuffers(1, &vbo_lines);
    glDeleteProgram(shader_program);
}

// --- Scatter Plot (OpenGL) ---
void PlotArea::plotScatterGL(const std::string& variant, int width, int height) {
  Color color = Packet::ColorMap[variant];
  if (color.alpha <= 0.0) return;

  TraceDatabase& database = TraceDatabase::instance();
  const auto& buffer = database[id].entries(variant);
  if (buffer.empty()) return;

  std::vector<float> points;
  for (int i = 0; i < buffer.size(); i++) {
    const auto& entry = buffer.at(i);
    float x = static_cast<float>(entry.first);  // Raw x (timestamp)
    float y = static_cast<float>(entry.second); // Raw y (count)
    points.insert(points.end(), {x, y, static_cast<float>(color.red), static_cast<float>(color.green), static_cast<float>(color.blue), static_cast<float>(color.alpha)});
  }

  glBindVertexArray(vao_points);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_points);
  glBufferData(GL_ARRAY_BUFFER, points.size() * sizeof(float), points.data(), GL_DYNAMIC_DRAW);
  glUniform1f(glGetUniformLocation(shader_program, "uRadius"), 2.0f);
  glDrawArrays(GL_POINTS, 0, points.size() / 6);  // Fixed: use actual point count
  glBindVertexArray(0);
}

// --- Timemarkers (OpenGL) ---
void PlotArea::plotTimemarkersGL() {
  TimemarkerCollection& collection = TimemarkerCollection::instance();
    TraceDatabase& database = TraceDatabase::instance();
    double x_min = database.minTimestamp();
    double x_max = database.maxTimestamp();
    double x_interval = x_max - x_min;

    std::vector<float> lines;
    for (const auto& marker : collection) {
        const Color& color = marker.getColor();
        if (color.alpha <= 0.0) continue;

        float x = adaptX(static_cast<double>(marker.getTime()), x_min, x_interval);
        float y1 = 0.0f;
        float y2 = tracker.plot.height;

        lines.insert(lines.end(), {x, y1, color.red, color.green, color.blue, color.alpha});
        lines.insert(lines.end(), {x, y2, color.red, color.green, color.blue, color.alpha});
    }

    glBindVertexArray(vao_lines);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_lines);
    glBufferData(GL_ARRAY_BUFFER, lines.size() * sizeof(float), lines.data(), GL_DYNAMIC_DRAW);
    glDrawArrays(GL_LINES, 0, lines.size() / 6);
    glBindVertexArray(0);
}

// --- Curves (OpenGL) ---
void PlotArea::plotCurveGL(const std::string& variant) {
    Color color = Packet::ColorMap[variant];
    //if (color.alpha <= 0.0) return;

    TraceDatabase& database = TraceDatabase::instance();
    double x_min = database.minTimestamp();
    double x_max = database.maxTimestamp();
    double x_interval = x_max - x_min;
    double y_min = database.minCount();
    double y_max = database.maxCount();
    double y_interval = y_max - y_min;

    const auto& buffer = database[id].entries(variant);
    if (buffer.size() <= 1) return; // Single point handled by scatter

    std::vector<float> lines;
    for (int i = 0; i < buffer.size(); i++) {
        const auto& entry = buffer.at(i);
        float x = (adaptX(entry.first, x_min, x_interval) - tracker.plot.width / 2.0f) / (tracker.plot.width / 2.0f);
        float y = (adaptY(entry.second, y_min, y_interval) - tracker.plot.height / 2.0f) / (tracker.plot.height / 2.0f);
        lines.insert(lines.end(), {x, y, color.red, color.green, color.blue, color.alpha});
    }

    glBindVertexArray(vao_lines);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_lines);
    glBufferData(GL_ARRAY_BUFFER, lines.size() * sizeof(float), lines.data(), GL_DYNAMIC_DRAW);
    glDrawArrays(GL_LINE_STRIP, 0, lines.size() / 6);
    glBindVertexArray(0);
}

// --- Axes (OpenGL) ---
void PlotArea::drawAxesGL() {
  const int nticks = 10;
    const float tick_len = 5.0f;

    std::vector<float> lines;

    // X axis (horizontal line)
    lines.insert(lines.end(), {0.0f, 0.0f, 0.2f, 0.2f, 0.2f, 1.0f});
    lines.insert(lines.end(), {tracker.plot.width, 0.0f, 0.2f, 0.2f, 0.2f, 1.0f});

    // Y axis (vertical line)
    lines.insert(lines.end(), {0.0f, 0.0f, 0.2f, 0.2f, 0.2f, 1.0f});
    lines.insert(lines.end(), {0.0f, tracker.plot.height, 0.2f, 0.2f, 0.2f, 1.0f});

    // X ticks
    for (int i = 0; i <= nticks; i++) {
        float tx = (i * tracker.plot.width) / nticks;
        lines.insert(lines.end(), {tx, 0.0f, 0.2f, 0.2f, 0.2f, 1.0f});
        lines.insert(lines.end(), {tx, tick_len, 0.2f, 0.2f, 0.2f, 1.0f});
    }

    // Y ticks
    for (int i = 0; i <= nticks; i++) {
        float ty = (i * tracker.plot.height) / nticks;
        lines.insert(lines.end(), {0.0f, ty, 0.2f, 0.2f, 0.2f, 1.0f});
        lines.insert(lines.end(), {-tick_len, ty, 0.2f, 0.2f, 0.2f, 1.0f});
    }

    glBindVertexArray(vao_lines);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_lines);
    glBufferData(GL_ARRAY_BUFFER, lines.size() * sizeof(float), lines.data(), GL_DYNAMIC_DRAW);
    glDrawArrays(GL_LINES, 0, lines.size() / 6);
    glBindVertexArray(0);
}

gboolean PlotArea::cOnScroll(GtkEventControllerScroll* controller, double dx, double dy, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  return self->onScroll(dy);
}

gboolean PlotArea::onScroll(double dy) {
  double old_scale = tracker.viewport.scale;
  double new_scale = std::clamp(old_scale * (dy < 0 ? 1.3 : 0.7), 1.0, 50.0);

  if (new_scale != old_scale) {
    double scale_ratio = new_scale / old_scale;
    double mouse_x = tracker.mouse.current.x;
    double mouse_y = tracker.mouse.current.y;

    // Adjust offset to keep mouse on same world point
    tracker.viewport.offset.x = mouse_x - (mouse_x - tracker.viewport.offset.x) * scale_ratio;
    tracker.viewport.offset.y = mouse_y - (mouse_y - tracker.viewport.offset.y) * scale_ratio;

    tracker.viewport.scale = static_cast<float>(new_scale);
    clampOffset();
    tracker.update();
  }

  return TRUE;
}

void PlotArea::cOnMotion(GtkEventControllerMotion* controller, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onMotion(x, y);
}

void PlotArea::onMotion(double x, double y) {
  // Convert to NDC [-1, 1]
  float new_x = static_cast<float>((2.0 * x / tracker.plot.width) - 1.0);
  float new_y = static_cast<float>(-((2.0 * y / tracker.plot.height) - 1.0));

  // Handling drag
  if (tracker.mouse.dragging) {
    float delta_x = new_x - tracker.mouse.last.x;
    float delta_y = new_y - tracker.mouse.last.y;

    // FIX: Subtract delta to pan in correct direction
    tracker.viewport.offset.x += delta_x;
    tracker.viewport.offset.y += delta_y;

    clampOffset();
    tracker.update();
  }

  // Update current position for next frame
  tracker.mouse.current.x = new_x;
  tracker.mouse.current.y = new_y;

  // Update last position when dragging
  if (tracker.mouse.dragging) {
    tracker.mouse.last.x = new_x;
    tracker.mouse.last.y = new_y;
  }
}

void PlotArea::cOnDialogResponse(GtkDialog* _, int response_id, gpointer user_data) {
  TimemarkerDialog* self = static_cast<TimemarkerDialog*>(user_data);
  self->onDialogResponse(response_id);
  delete self;
}


void PlotArea::cOnDialogResponseUpdate(GtkDialog* _, int _1, gpointer user_data) {
  PlotAreaTracker* self = static_cast<PlotAreaTracker*>(user_data);
  self->update();
}


void PlotArea::cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  bool right = GDK_BUTTON_SECONDARY == gtk_gesture_single_get_current_button(GTK_GESTURE_SINGLE(gesture));
  self->onButtonPress(right, x, y);
}

void PlotArea::onButtonPress(bool right, double x, double y) {
  // Convert to NDC
  float ndc_x = static_cast<float>((2.0 * x / tracker.plot.width) - 1.0);
  float ndc_y = static_cast<float>(-((2.0 * y / tracker.plot.height) - 1.0));

  TraceDatabase& database = TraceDatabase::instance();
  float x_min = static_cast<float>(database.minTimestamp());
  float x_max = static_cast<float>(database.maxTimestamp());
  float y_min = static_cast<float>(database.minCount());
  float y_max = static_cast<float>(database.maxCount());

  float x_range = x_max - x_min;
  float y_range = y_max - y_min;
  float x_center = (x_min + x_max) / 2.0f;
  float y_center = (y_min + y_max) / 2.0f;

  float s = tracker.viewport.scale;
  float xo = tracker.viewport.offset.x;
  float yo = tracker.viewport.offset.y;

  // Convert NDC to data coordinates
  float world_x = x_center + (ndc_x - xo) * x_range / (2.0f * s);
  float world_y = y_center + (ndc_y - yo) * y_range / (2.0f * s);

  // Check if mouse is within plot area (NDC is always [-1,1] for the widget)
  bool isInframe = true; // Since NDC coordinates are already clipped to the widget

  if (right) {
    if (isInframe) {
      GtkWindow* window = GTK_WINDOW(gtk_widget_get_root(GTK_WIDGET(parent)));
      TimemarkerDialog* timemarker = new TimemarkerDialog(GTK_WINDOW(window), static_cast<double>(world_x));
      g_signal_connect(timemarker->parent, "response", G_CALLBACK(PlotArea::cOnDialogResponse), timemarker);
      g_signal_connect(timemarker->parent, "response", G_CALLBACK(PlotArea::cOnDialogResponseUpdate), &tracker);
      gtk_window_present(GTK_WINDOW(timemarker->parent));
    }
  }
  else {
    // Dragging management
    tracker.mouse.dragging = true;
    tracker.mouse.last.x = ndc_x;
    tracker.mouse.last.y = ndc_y;

    if (isInframe) {
      std::string description = "";
      auto candidates = getPointsInRadius(static_cast<uint64_t>(world_x), static_cast<uint32_t>(world_y), 2.0f/sqrt(s));
      for (const auto& candidate : candidates) {
        std::string msg = database[id].find(candidate.first, candidate.second);
        if (msg != "") {
          description += msg + "\n";
        }
      }
      if (description != "") {
        Description::instance().reset();
        Description::instance().add(description);
        Description::instance().update();
      }
    }
  }
}

void PlotArea::cOnButtonRelease(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onButtonRelease();
}

void PlotArea::onButtonRelease() {
  tracker.mouse.dragging = false;
}

void PlotArea::clampOffset() {
  double s = tracker.viewport.scale; // Use double for consistency
  // Clamp to prevent showing empty space beyond data boundaries
  tracker.viewport.offset.x = std::clamp(tracker.viewport.offset.x, 1.0 - s, s - 1.0);
  tracker.viewport.offset.y = std::clamp(tracker.viewport.offset.y, 1.0 - s, s - 1.0);
}

const double PlotArea::adaptX(const double& value, const double& min, const double& interval) const {
  return (value-min)/interval*tracker.plot.width;
}

const double PlotArea::adaptY(const double& value, const double& min, const double& interval) const {
  return tracker.plot.height-((value-min)/interval*tracker.plot.height);
}

std::vector<std::pair<uint64_t,uint32_t>> PlotArea::getPointsInRadius(uint64_t x, uint32_t y, double r) {
  std::vector<std::pair<uint64_t, uint32_t>> result;
  // Define region of interest
  uint64_t xmin = std::ceil(x-r);
  uint64_t xmax = std::floor(x+r);
  uint32_t ymin = std::ceil(y-r);
  uint32_t ymax = std::floor(y+r);
  // Look-up all coordinate in grid
  double r2 = r*r;
  for (uint64_t i = xmin; i <= xmax; i++) {
    for (uint32_t j = ymin; j <= ymax; j++) {
      double dx = static_cast<double>(i-x);
      double dy = static_cast<double>(j-y);
      if (((dx*dx)+(dy*dy)) <= r2) {
        result.emplace_back(i, j);
      }
    }
  }
  return result;
}
