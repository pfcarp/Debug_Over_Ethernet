#pragma once


#include <gtk/gtk.h>
#include <cairo.h>


#include "Graph.hpp"


class GraphArea {

  private:
    struct {
      double length = 10.0;
      double width  =  6.0;
    } arrow;
    double scale = 1;
    cairo_t* cairo;

    void onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height);
    static void cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data);
    void drawNodes();
    void drawEdges();
    void setBackground();

  public:
    GtkWidget* parent;
    Graph* graph;

    GraphArea(unsigned width, unsigned height, Graph* graph);

};
