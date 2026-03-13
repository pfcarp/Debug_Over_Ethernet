#include "PlotArea.hpp"


#include <cmath>
#include <cstddef>
#include <cstdint>
#include <algorithm>


#include "Description.hpp"
#include "TimemarkerDialog.hpp"
#include "TimemarkerCollection.hpp"
#include "TraceDatabase.hpp"


const char* xlabel = "Time (CC)";


PlotArea::PlotArea(unsigned width, unsigned height, uint32_t id): dimensions({width, height}), id(id) {
  parent = gtk_drawing_area_new();
  gtk_widget_set_hexpand(parent, TRUE);
  gtk_widget_set_vexpand(parent, TRUE);
  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(parent), cOnDraw, this, NULL);
  // Scroll
  GtkEventController* scroll = gtk_event_controller_scroll_new(GTK_EVENT_CONTROLLER_SCROLL_VERTICAL);
  g_signal_connect(scroll, "scroll", G_CALLBACK(PlotArea::cOnScroll), this);
  gtk_widget_add_controller(parent, scroll);
  // Mouse
  GtkEventController* motion = gtk_event_controller_motion_new();
  g_signal_connect(motion, "motion", G_CALLBACK(cOnMotion), this);
  gtk_widget_add_controller(parent, motion);
  // Click
  GtkGestureClick* click = GTK_GESTURE_CLICK(gtk_gesture_click_new());
  gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click), 0);
  gtk_widget_add_controller(parent, GTK_EVENT_CONTROLLER(click));
  g_signal_connect(click, "pressed", G_CALLBACK(cOnButtonPress), this);
  g_signal_connect(click, "released", G_CALLBACK(cOnButtonRelease), this);
}


void PlotArea::cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onDraw(area, cr, width, height);
}

void PlotArea::onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height) {
  // update geometries
  cairo = cr;
  dimensions.width = width;
  dimensions.height = height;
  plot.width = dimensions.width-(1.5*plot.x);
  plot.height = dimensions.height-(2.0*plot.y);
  // draw data
  cairo_save(cairo);
  setBackground();
  ////
  cairo_rectangle(cairo, plot.x, plot.y, plot.width, plot.height);
  cairo_clip(cairo);
  ////
  cairo_translate(cairo, plot.x, plot.y);
  cairo_translate(cairo, viewport.offset.x, viewport.offset.y);
  cairo_scale(cairo, viewport.scale, viewport.scale);
  ////
  plotTimemarkers();
  const auto& variants = TraceDatabase::instance()[id].getVariants();
  for (const std::string& variant : variants) {
    plotScatter(variant);
  }
  ////
  cairo_restore(cr);
  // axes
  cairo_save(cairo);
  drawAxes();
  drawTimemarkerHeaders();
  cairo_restore(cairo);
}


gboolean PlotArea::cOnScroll(GtkEventControllerScroll* controller, double dx, double dy, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  return self->onScroll(dy);
}

gboolean PlotArea::onScroll(double dy) {
  world.width  = dimensions.width;
  world.height = dimensions.height;
  // Convert mouse position to plot-local coordinates
  double px = mouse.current.x-(plot.x);
  double py = mouse.current.y-(plot.y);
  // Ignore scroll outside plot area
  if (px < 0 || px > plot.width || py < 0 || py > plot.height)
      return FALSE;
  // Convert cursor to world coordinates BEFORE zoom
  double world_x = (px-viewport.offset.x)/viewport.scale;
  double world_y = (py-viewport.offset.y)/viewport.scale;
  // Update zoom
  if      (dy < 0) viewport.scale = std::clamp(viewport.scale+0.25, 1.0, 50.0);
  else if (dy > 0) viewport.scale = std::clamp(viewport.scale-0.25, 1.0, 50.0);
  // Recompute offset so the world point stays under the cursor
  viewport.offset.x = px-(world_x*viewport.scale);
  viewport.offset.y = py-(world_y*viewport.scale);
  // Clamp offset to prevent empty areas
  clampOffset();
  gtk_widget_queue_draw(parent);
  return TRUE;
}


void PlotArea::cOnMotion(GtkEventControllerMotion* controller, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onMotion(x, y);
}

void PlotArea::onMotion(double x, double y) {
  // Keep track of current position (for scroll)
  mouse.current.x = x;
  mouse.current.y = y;
  // Handling drag
  if (mouse.dragging) [[unlikely]] {
    viewport.offset.x += x-mouse.last.x;
    viewport.offset.y += y-mouse.last.y;
    mouse.last.x = x;
    mouse.last.y = y;
    clampOffset();   // same clamping logic used in zoom
    gtk_widget_queue_draw(parent);
  }
}


void PlotArea::cOnDialogResponse(GtkDialog* dialog, int response_id, gpointer user_data) {
  TimemarkerDialog* self = static_cast<TimemarkerDialog*>(user_data);
  self->onDialogResponse(response_id);
  delete self;
}


void PlotArea::cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  bool right = GDK_BUTTON_SECONDARY == gtk_gesture_single_get_current_button(GTK_GESTURE_SINGLE(gesture));
  self->onButtonPress(right, x, y);
}

void PlotArea::onButtonPress(bool right, double x, double y) {
  // Convert mouse position to plot-local coordinates
  double px = mouse.current.x-(plot.x);
  double py = mouse.current.y-(plot.y);
  bool isInframe = (px >= 0) && (px <= plot.width) && (py >= 0) && (py <= plot.height);
  // Convert cursor to world coordinates BEFORE zoom
  TraceDatabase& database = TraceDatabase::instance();
  //// X axis
  auto xmin = database.minTimestamp();
  auto xmax = database.maxTimestamp();
  double visible_width_x = (xmax-xmin)/viewport.scale;
  double world_x_min = xmin+(((-viewport.offset.x/plot.width)/viewport.scale)*xmax);
  double local_x = (px/plot.width)*visible_width_x;
  double global_x = world_x_min+local_x;
  //// Y axis
  auto ymin = database.minCount();
  auto ymax = database.maxCount();
  double visible_width_y = (ymax-ymin)/viewport.scale;
  double world_y_min = ymin+ymax-((((plot.height-viewport.offset.y)/plot.height)/viewport.scale)*ymax);
  double local_y = (1.0-(py/plot.height))*visible_width_y;
  double global_y = world_y_min+local_y;
  if (right) {
    if (isInframe) {
      GtkWindow* window = GTK_WINDOW(gtk_widget_get_root(GTK_WIDGET(parent)));
      TimemarkerDialog* timemarker = new TimemarkerDialog(GTK_WINDOW(window), global_x);
      g_signal_connect(timemarker->parent, "response", G_CALLBACK(PlotArea::cOnDialogResponse), timemarker);
      gtk_window_present(GTK_WINDOW(timemarker->parent));
    }
  }
  else {
    // Dragging management
    mouse.dragging = true;
    mouse.last.x = x;
    mouse.last.y = y;
    // Info lookup management
    //// Ignore scroll outside plot area
    if (isInframe) {
      ////// handle all matches
      std::string description = "";
      auto candidates = getPointsInRadius(global_x, global_y, 2.0/sqrt(viewport.scale));
      for (const auto& candidate : candidates) {
        std::string msg = database[id].find(candidate.first, candidate.second);
        if (msg != "") {
          description += msg+"\n";
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
  mouse.dragging = false;
}


void PlotArea::setBackground() {
  cairo_set_source_rgb(cairo, 1, 1, 1);
  cairo_paint(cairo);
}


void PlotArea::handleZoom() {
  cairo_translate(cairo, viewport.offset.x, viewport.offset.y);
  cairo_scale(cairo, viewport.scale, viewport.scale);
}


void PlotArea::clampOffset() {
  viewport.offset.x = std::clamp(viewport.offset.x, -(plot.width)*(viewport.scale-1), 0.0);
  viewport.offset.y = std::clamp(viewport.offset.y, -(plot.height)*(viewport.scale-1), 0.0);
}


void PlotArea::plotTimemarkers() {
  TimemarkerCollection& collection = TimemarkerCollection::instance();
  TraceDatabase& database = TraceDatabase::instance();
  double x_min = database.minTimestamp();
  double x_max = database.maxTimestamp();
  double x_interval = x_max-x_min;
  for (const auto& marker : collection) {
    const Color& color = marker.getColor();
    if (color.alpha > 0.0) {
      // Define line setup
      cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
      cairo_set_line_width(cairo, 1.5/viewport.scale);
      // Draw vertical line
      double x = adaptX(static_cast<double>(marker.getTime()), x_min, x_interval);
      cairo_move_to(cairo, x, 0.0);
      cairo_line_to(cairo, x, plot.height);
    }
  }
  // Actually draw
  cairo_stroke(cairo);
}


void PlotArea::plotCurve(const std::string& variant) {
  Color color = Packet::ColorMap[variant];
  // Bother drawing iff the color is not completely transparent
  if (color.alpha > 0.0) {
    // parameter
    TraceDatabase& database = TraceDatabase::instance();
    double x_min = database.minTimestamp();
    double x_max = database.maxTimestamp();
    double x_interval = x_max-x_min;
    double y_min = database.minCount();
    double y_max = database.maxCount();
    double y_interval = y_max-y_min;
    // Define line setup
    cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
    cairo_set_line_width(cairo, 2.0/viewport.scale);
    // Draw each curve
    const auto& buffer = database[id].entries(variant);
    if (buffer.size() == 1) {
      const auto& entry = buffer.at(0);
      cairo_arc(cairo, adaptX(entry.first, x_min, x_interval), adaptY(entry.second, y_min, y_interval), 2.0/sqrt(viewport.scale), 0, 2*M_PI);
      cairo_fill(cairo);
    }
    else if (buffer.size() > 1) {
      const auto& entry = buffer.at(0);
      cairo_move_to(cairo, adaptX(entry.first, x_min, x_interval), adaptY(entry.second, y_min, y_interval));
      for (int i = 1; i < buffer.size(); i++) {
        const auto& entry = buffer.at(i);
        cairo_line_to(cairo, adaptX(entry.first, x_min, x_interval), adaptY(entry.second, y_min, y_interval));
      }
    }
    // Actually draw
    cairo_stroke(cairo);
  }
}

void PlotArea::plotScatter(const std::string& variant) {
  Color color = Packet::ColorMap[variant];
  // Bother drawing iff the color is not completely transparent
  if (color.alpha > 0.0) {
    // parameter
    TraceDatabase& database = TraceDatabase::instance();
    double x_min = database.minTimestamp();
    double x_max = database.maxTimestamp();
    double x_interval = x_max-x_min;
    double y_min = database.minCount();
    double y_max = database.maxCount();
    double y_interval = y_max-y_min;
    // Define line setup
    cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
    // Draw each curve
    const auto& buffer = database[id].entries(variant);
    for (int i = 0; i < buffer.size(); i++) {
      const auto& entry = buffer.at(i);
      cairo_arc(cairo, adaptX(entry.first, x_min, x_interval), adaptY(entry.second, y_min, y_interval), 2.0/sqrt(viewport.scale), 0, 2*M_PI);
      cairo_fill(cairo);
    }
    // Actually draw
    cairo_stroke(cairo);
  }
}

void PlotArea::drawTimemarkerHeaders() {
  TraceDatabase& database = TraceDatabase::instance();
  // Global parameters
  const double padding = 8.0;
  double x_min = database.minTimestamp();
  double x_max = database.maxTimestamp();
  double x_visible_range_min = x_min+(((-viewport.offset.x/plot.width)/viewport.scale)*x_max);
  double x_visible_range_max = x_visible_range_min+((x_max-x_min)/viewport.scale);
  // Loop over visible (i.e., in-range) time markers
  TimemarkerCollection& collection = TimemarkerCollection::instance();
  collection.setScope(x_visible_range_min, x_visible_range_max);
  for (const auto& marker : collection) {
    const Color& color = marker.getColor();
    if (color.alpha > 0.0) {
      double x = plot.x+adaptX(static_cast<double>(marker.getTime()), x_visible_range_min, x_visible_range_max-x_visible_range_min);
      // Compute inscribed text dimensions
      cairo_select_font_face(cairo, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
      cairo_set_font_size(cairo, 11);
      cairo_text_extents_t ext;
      cairo_text_extents(cairo, marker.getName().c_str(), &ext);
      // Rectangle
      //// Compute parameters
      double rect_w = ext.width+(2*padding);
      double rect_h = ext.height+(2*padding);
      double rect_x = x-(rect_w/2.0);
      double rect_y = plot.y-rect_h;
      //// Draw
      cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
      cairo_rectangle(cairo, rect_x, rect_y, rect_w, rect_h);
      cairo_fill(cairo);
      // Label
      //// Compute text position
      double text_x = x-(ext.width/2.0)-ext.x_bearing;
      double text_y = rect_y+((padding+ext.height)/2.0)-ext.y_bearing;
      //// Draw text
      cairo_set_source_rgb(cairo, 1, 1, 1);
      cairo_move_to(cairo, text_x, text_y);
      cairo_show_text(cairo, marker.getName().c_str());
    }
  }
  collection.resetScope();
}

void PlotArea::drawAxes() {
  TraceDatabase& database = TraceDatabase::instance();
  const int nticks = 10;
  const double tick_len = 5.0;

  cairo_set_source_rgb(cairo, 0.2, 0.2, 0.2);
  cairo_set_line_width(cairo, 1.0);
  cairo_select_font_face(cairo, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cairo, 10.0);

  // --- X axis ---
  cairo_move_to(cairo, plot.x, dimensions.height-plot.y);
  cairo_line_to(cairo, plot.x+plot.width, dimensions.height-plot.y);
  cairo_stroke(cairo);
  // X-axis label (centered below)
  cairo_set_font_size(cairo, 12.0);
  cairo_text_extents_t extents;
  cairo_text_extents(cairo, xlabel, &extents);
  cairo_move_to(cairo, plot.x+(plot.width-extents.width)/2, dimensions.height-10);
  cairo_show_text(cairo, xlabel);
  // X ticks & labels (between edges)
  auto xmin = database.minTimestamp();
  auto xmax = database.maxTimestamp();
  double visible_width_x = (xmax-xmin)/viewport.scale;
  double world_x_min = xmin+(((-viewport.offset.x/plot.width)/viewport.scale)*xmax);
  for (int i = 0; i <= nticks; i++) {
    double t = (double)i/nticks;
    double tx = plot.x+t*plot.width;
    // Tick mark
    cairo_move_to(cairo, tx, dimensions.height-plot.y);
    cairo_line_to(cairo, tx, dimensions.height-plot.y+tick_len);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    snprintf(label, sizeof(label), "%.1f", world_x_min+t*visible_width_x);
    cairo_move_to(cairo, tx-10, dimensions.height-plot.y+15);
    cairo_show_text(cairo, label);
  }

  // --- Y axis ---
  cairo_move_to(cairo, plot.x, dimensions.height-plot.y);
  cairo_line_to(cairo, plot.x, dimensions.height-plot.y-plot.height);
  cairo_stroke(cairo);
  // Y ticks & labels
  auto ymin = database.minCount();
  auto ymax = database.maxCount();
  double visible_width_y = (ymax-ymin)/viewport.scale;
  double world_y_min = ymin+ymax-((((plot.height-viewport.offset.y)/plot.height)/viewport.scale)*ymax);
  for (int i = 0; i <= nticks; i++) {
    double t = ((double)(nticks-i))/nticks;
    double ty = dimensions.height-plot.y-t*plot.height;
    // Tick mark
    cairo_move_to(cairo, plot.x, ty);
    cairo_line_to(cairo, plot.x-tick_len, ty);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    snprintf(label, sizeof(label), "%.1f", world_y_min+t*visible_width_y);
    cairo_move_to(cairo, plot.x-35, ty+3);
    cairo_show_text(cairo, label);
  }
}


const double PlotArea::adaptX(const double& value, const double& min, const double& interval) const {
  return (value-min)/interval*plot.width;
}

const double PlotArea::adaptY(const double& value, const double& min, const double& interval) const {
  return plot.height-((value-min)/interval*plot.height);
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
