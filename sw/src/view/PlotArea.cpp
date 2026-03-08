#include "PlotArea.hpp"


#include <cmath>
#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <iostream>
#include <vector>


const char* xlabel = "Time (CC)";


PlotArea::PlotArea(unsigned width, unsigned height, PacketFactory& factory): dimensions({width, height}), factory(factory) {
  parent = gtk_drawing_area_new();
  gtk_widget_set_hexpand(parent, TRUE);
  gtk_widget_set_vexpand(parent, TRUE);
  gtk_widget_set_size_request(parent, width, height);
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
  gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click), 1);
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
  plot.height = dimensions.height-(1.5*plot.y);
  // draw data
  cairo_save(cairo);
  setBackground();
  ////
  cairo_rectangle(cairo, plot.x, plot.y*(1.5-1), plot.width, plot.height);
  cairo_clip(cairo);
  ////
  cairo_translate(cairo, plot.x, plot.y*(1.5-1));
  cairo_translate(cairo, viewport.offset.x, viewport.offset.y);
  cairo_scale(cairo, viewport.scale, viewport.scale);
  ////
  for (const std::string& variant : factory.map.getVariants()) {
    plotScatter(variant);
  }
  ////
  cairo_restore(cr);
  // axes
  cairo_save(cairo);
  drawAxes();
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
  double py = mouse.current.y-(plot.y*0.5);
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


void PlotArea::cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onButtonPress(x, y);
}

void PlotArea::onButtonPress(double x, double y) {
  mouse.dragging = true;
  mouse.last.x = x;
  mouse.last.y = y;
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
  double min_x = dimensions.width - world.width * viewport.scale;
  double max_x = 0;
  viewport.offset.x = std::clamp(viewport.offset.x, min_x, max_x);
  double min_y = dimensions.height - world.height * viewport.scale;
  double max_y = 0;
  viewport.offset.y = std::clamp(viewport.offset.y, min_y, max_y);
}


void PlotArea::plotCurve(const std::string& variant) {
  Color color = Packet::ColorMap[variant];
  // Bother drawing iff the color is not completely transparent
  if (color.alpha > 0.0) {
    // Define line setup
    cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
    cairo_set_line_width(cairo, 2.0/viewport.scale);
    // Draw each curve
    const auto& buffer = factory.map.entries(variant);
    if (buffer.size() == 1) {
      const auto& entry = buffer.at(0);
      cairo_arc(cairo, adaptX(entry.first), adaptY(entry.second), 2.0/viewport.scale, 0, 2*M_PI);
      cairo_fill(cairo);
    }
    else if (buffer.size() > 1) {
      const auto& entry = buffer.at(0);
      cairo_move_to(cairo, adaptX(entry.first), adaptY(entry.second));
      for (int i = 1; i < buffer.size(); i++) {
        const auto& entry = buffer.at(i);
        cairo_line_to(cairo, adaptX(entry.first), adaptY(entry.second));
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
    // Define line setup
    cairo_set_source_rgba(cairo, color.red, color.green, color.blue, color.alpha);
    // Draw each curve
    const auto& buffer = factory.map.entries(variant);
    for (int i = 0; i < buffer.size(); i++) {
      const auto& entry = buffer.at(i);
      cairo_arc(cairo, adaptX(entry.first), adaptY(entry.second), 2.0/viewport.scale, 0, 2*M_PI);
      cairo_fill(cairo);
    }
    // Actually draw
    cairo_stroke(cairo);
  }
}

void PlotArea::drawAxes() {
  const int nticks = 10;
  const double tick_len = 5.0;

  cairo_set_source_rgb(cairo, 0.2, 0.2, 0.2);
  cairo_set_line_width(cairo, 1.0/viewport.scale);
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
  for (int i = 0; i <= nticks; i++) {
    double t = (double)i/nticks;
    double tx = plot.x+t*plot.width;
    // Tick mark
    cairo_move_to(cairo, tx, dimensions.height-plot.y);
    cairo_line_to(cairo, tx, dimensions.height-plot.y+tick_len);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    auto xmin = factory.map.minTimestamp();
    auto xmax = factory.map.maxTimestamp();
    snprintf(label, sizeof(label), "%.1f", xmin+t*(xmax-xmin));
    cairo_move_to(cairo, tx-10, dimensions.height-plot.y+15);
    cairo_show_text(cairo, label);
  }

  // --- Y axis ---
  cairo_move_to(cairo, plot.x, dimensions.height-plot.y);
  cairo_line_to(cairo, plot.x, dimensions.height-plot.y-plot.height);
  cairo_stroke(cairo);
  // Y ticks & labels
  for (int i = 0; i <= nticks; i++) {
    double t = (double)i/nticks;
    double ty = dimensions.height-plot.y-t*plot.height;
    // Tick mark
    cairo_move_to(cairo, plot.x, ty);
    cairo_line_to(cairo, plot.x-tick_len, ty);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    auto ymin = factory.map.minCount();
    auto ymax = factory.map.maxCount();
    snprintf(label, sizeof(label), "%.1f", ymin+t*(ymax-ymin));
    cairo_move_to(cairo, plot.x-35, ty+3);
    cairo_show_text(cairo, label);
  }
}


double PlotArea::adaptX(double value) {
  double min = factory.map.minTimestamp();
  double max = factory.map.maxTimestamp();
  return (value-min)/(max-min)*plot.width;
}

double PlotArea::adaptY(double value) {
  double min = factory.map.minCount();
  double max = factory.map.maxCount();
  return plot.height-((value-min)/(max-min)*plot.height);
}
