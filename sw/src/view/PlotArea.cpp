#include "PlotArea.hpp"


#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <vector>


const char* xlabel = "Time (CC)";


PlotArea::PlotArea(unsigned width, unsigned height, PacketFactory* factory): dimensions({width, height}), factory(factory) {
  parent = gtk_drawing_area_new();
  gtk_widget_set_hexpand(parent, TRUE);
  gtk_widget_set_vexpand(parent, TRUE);
  gtk_widget_set_size_request(parent, width, height);
  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(parent), cOnDraw, this, NULL);
}


void PlotArea::cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data) {
  PlotArea* self = static_cast<PlotArea*>(user_data);
  self->onDraw(area, cr, width, height);
}

void PlotArea::onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height) {
  dimensions.width = width;
  dimensions.height = height;
  cairo = cr;
  setBackground();
  for (const std::string& variant : factory->map.getVariants()) {
    Color color = Packet::ColorMap[variant];
    Trace trace = factory->map.entries(variant);
    plotCurve(&color, &trace);
  }
  drawAxes();
}


void PlotArea::setBackground() {
  cairo_set_source_rgb(cairo, 1, 1, 1);
  cairo_paint(cairo);
}


void PlotArea::plotCurve(const Color* color, const Trace* trace) {
  // Define line setup
  cairo_set_source_rgba(cairo, color->red, color->green, color->blue, color->alpha);
  cairo_set_line_width(cairo, 2.0);
  // Draw each curve
  const auto& buffer = trace->entries(true);
  if (buffer.size()) {
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

void PlotArea::plotScatter(const Color* color, const Trace* trace) {
  // Define line setup
  cairo_set_source_rgba(cairo, color->red, color->green, color->blue, color->alpha);
  // Draw each curve
  const auto& buffer = trace->entries(false);
  for (int i = 0; i < buffer.size(); i++) {
    const auto& entry = buffer.at(i);
    cairo_arc(cairo, adaptX(entry.first), adaptY(entry.second), 2.0, 0, 2*M_PI);
    cairo_fill(cairo);
  }
  // Actually draw
  cairo_stroke(cairo);
}

void PlotArea::drawAxes() {
  const int nticks = 10;
  const double tick_len = 5.0;

  cairo_save(cairo);
  cairo_set_source_rgb(cairo, 0.2, 0.2, 0.2);
  cairo_set_line_width(cairo, 1.0);
  cairo_select_font_face(cairo, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cairo, 10.0);

  // --- X axis ---
  cairo_move_to(cairo, margin.left, dimensions.height-margin.bottom);
  cairo_line_to(cairo, dimensions.width-margin.right, dimensions.height-margin.bottom);
  cairo_stroke(cairo);
  // X-axis label (centered below)
  cairo_set_font_size(cairo, 12.0);
  cairo_text_extents_t extents;
  cairo_text_extents(cairo, xlabel, &extents);
  cairo_move_to(cairo, margin.left+(plotWidth()-extents.width)/2, dimensions.height-10);
  cairo_show_text(cairo, xlabel);
  // X ticks & labels (between edges)
  for (int i = 0; i <= nticks; i++) {
    double t = (double)i/nticks;
    double tx = margin.left+t*plotWidth();
    // Tick mark
    cairo_move_to(cairo, tx, dimensions.height-margin.bottom);
    cairo_line_to(cairo, tx, dimensions.height-margin.bottom+tick_len);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    auto xmin = factory->map.minTimestamp();
    auto xmax = factory->map.maxTimestamp();
    snprintf(label, sizeof(label), "%.1f", xmin+t*(xmax-xmin));
    cairo_move_to(cairo, tx-10, dimensions.height-margin.bottom+15);
    cairo_show_text(cairo, label);
  }

  // --- Y axis ---
  cairo_move_to(cairo, margin.left, dimensions.height-margin.bottom);
  cairo_line_to(cairo, margin.left, margin.top);
  cairo_stroke(cairo);
  // Y ticks & labels
  for (int i = 0; i <= nticks; i++) {
    double t = (double)i/nticks;
    double ty = dimensions.height-margin.bottom-t*plotHeight();
    // Tick mark
    cairo_move_to(cairo, margin.left, ty);
    cairo_line_to(cairo, margin.left-tick_len, ty);
    cairo_stroke(cairo);
    // Label slightly below tick
    char label[32];
    auto ymin = factory->map.minCount();
    auto ymax = factory->map.maxCount();
    snprintf(label, sizeof(label), "%.1f", ymin+t*(ymax-ymin));
    cairo_move_to(cairo, margin.left-35, ty+3);
    cairo_show_text(cairo, label);
  }

  cairo_restore(cairo);
}


double PlotArea::plotWidth() {
  return dimensions.width-margin.left-margin.right;
}

double PlotArea::plotHeight() {
  return dimensions.height-margin.top-margin.bottom;
}


double PlotArea::adaptX(double value) {
  double min = factory->map.minTimestamp();
  double max = factory->map.maxTimestamp();
  return margin.left+((value-min)/(max-min)*plotWidth());
}

double PlotArea::adaptY(double value) {
  double min = factory->map.minCount();
  double max = factory->map.maxCount();
  return dimensions.height-margin.bottom-((value-min)/(max-min)*plotHeight());
}
