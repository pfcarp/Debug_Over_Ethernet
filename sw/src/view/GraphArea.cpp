#include "GraphArea.hpp"


#include <cmath>


GraphArea::GraphArea(unsigned width, unsigned height, Graph* graph): graph(graph) {
  parent = gtk_drawing_area_new();
  gtk_widget_set_size_request(parent, width, height);
  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(parent), cOnDraw, this, NULL);
}


void GraphArea::cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data) {
  GraphArea* self = static_cast<GraphArea*>(user_data);
  self->onDraw(area, cr, width, height);
}

void GraphArea::onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height) {
  cairo = cr;
  // Scale diagram
  scale = fmin(width/graph->width, height/graph->height);
  cairo_translate(cairo, 0, height);
  cairo_scale(cairo, scale, -scale);
  // draw content
  drawEdges();
  drawNodes();
}


void GraphArea::drawNodes() {
  for (const auto& n : graph->nodes) {
    double radius = n.width/2.0;
    cairo_new_path(cairo);
    cairo_arc(cairo, n.x, n.y, radius, 0, 2*M_PI);
    cairo_set_source_rgb(cairo, n.color.red, n.color.green, n.color.blue);
    cairo_fill_preserve(cairo);
    // draw circle frame
    if (n.current) {
      cairo_set_source_rgb(cairo, 1.0, 0.0, 0.0);   // red border
      cairo_set_line_width(cairo, 3.0);
      cairo_stroke(cairo);
    }

    cairo_save(cairo);
    cairo_scale(cairo, 1, -1);
    cairo_set_source_rgb(cairo, 0, 0, 0);
    cairo_select_font_face(cairo, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size(cairo, 12);
    cairo_move_to(cairo, n.x-(n.name.size()*3), -n.y+4);
    cairo_show_text(cairo, n.name.c_str());
    cairo_restore(cairo);
  }
}


void GraphArea::drawEdges() {
  cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0);
  cairo_set_line_width(cairo, 1.0/scale);

  for (const auto& e : graph->edges) {
    if (e.points.size() < 2)
      continue;

    // Draw the spline/line itself
    cairo_new_path(cairo);
    cairo_move_to(cairo, e.points[0].first, e.points[0].second);
    for (size_t i = 1; i < e.points.size(); i++)
      cairo_line_to(cairo, e.points[i].first, e.points[i].second);
    cairo_stroke(cairo);

    // --- Draw arrowhead ---
    const auto& tail = e.points[e.points.size()-2];
    const auto& head = e.points[e.points.size()-1];
    double dx = head.first-tail.first;
    double dy = head.second-tail.second;
    double len = sqrt(dx*dx+dy*dy);
    if (len == 0) continue;

    // Normalize direction
    dx /= len;
    dy /= len;

    // Base of the arrow
    double bx = head.first-arrow.length*dx;
    double by = head.second-arrow.length*dy;

    // Perpendicular vector
    double px = -dy;
    double py = dx;

    // Compute triangle points
    double x1 = head.first;
    double y1 = head.second;
    double x2 = bx+px*arrow.width/2.0;
    double y2 = by+py*arrow.width/2.0;
    double x3 = bx-px*arrow.width/2.0;
    double y3 = by-py*arrow.width/2.0;

    cairo_new_path(cairo);
    cairo_move_to(cairo, x1, y1);
    cairo_line_to(cairo, x2, y2);
    cairo_line_to(cairo, x3, y3);
    cairo_close_path(cairo);

    cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0);
    cairo_fill(cairo);
  }
}

void GraphArea::setBackground() {
  cairo_set_source_rgb(cairo, 1, 1, 1);
  cairo_paint(cairo);
}
