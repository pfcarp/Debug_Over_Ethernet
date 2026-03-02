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
  for (const auto& packets : factory->packets) {
    plotCurve(&(packets.second));
  }
  drawAxes();
}


void PlotArea::setBackground() {
  cairo_set_source_rgb(cairo, 1, 1, 1);
  cairo_paint(cairo);
}


void PlotArea::plotCurve(const std::vector<Packet::Variant>* buffer) {
  // Define line setup
  cairo_set_source_rgba(cairo, 0.0, 0.0, 0.0, 1.0);
  cairo_set_line_width(cairo, 2.0);
  // Draw each curve
  if (buffer->size()) {
    const Packet::Variant& packet = buffer->at(0);
    cairo_move_to(cairo, adaptX(Packet::getTimestamp(packet)), adaptY(1));
    for (int i = 1; i < buffer->size(); i++) {
      const Packet::Variant& packet = buffer->at(i);
      cairo_line_to(cairo, adaptX(Packet::getTimestamp(packet)), adaptY(1));
    }
  }
  // Actually draw
  cairo_stroke(cairo);
}

void PlotArea::plotScatter(const std::vector<Packet::Variant>* buffer) {
  // Define line setup
  cairo_set_source_rgba(cairo, 0.0, 0.0, 0.0, 1.0);
  // Draw each curve
  for (int i = 0; i < buffer->size(); i++) {
    const Packet::Variant& packet = buffer->at(i);
    cairo_arc(cairo, adaptX(Packet::getTimestamp(packet)), adaptY(1), 2.0, 0, 2*M_PI);
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
    snprintf(label, sizeof(label), "%.1f", 0+t*(1.1-0));
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
    snprintf(label, sizeof(label), "%.1f", 0+t*(1.1-0));
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
  double min = 0;
  double max = factory->packets["Extension"].size();
  return margin.left+(value-min)/(max-min)*plotWidth();
}

double PlotArea::adaptY(double value) {
  double min = 0*1.1;
  double max = 1*1.1;
  return dimensions.height-margin.bottom-((value-min)/(max-min)*plotHeight());
}
