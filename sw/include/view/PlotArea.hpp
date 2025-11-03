#pragma once


#include <gtk/gtk.h>


#include "Collection.hpp"
#include "Buffer.hpp"


class PlotArea {

  private:
    const struct {
      double top    = 20;
      double bottom = 40;
      double left   = 50;
      double right  = 20;
    } margin;
    struct {
      double width;
      double height;
    } dimensions;
    cairo_t* cairo;

    inline double plotWidth();
    inline double plotHeight();
    inline double adaptX(double value);
    inline double adaptY(double value);
    void onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height);
    static void cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data);
    void setBackground();
    void drawAxes();
    void plotCurve(Buffer* buffer);
    void plotScatter(Buffer* buffer);

  public:
    GtkWidget* parent;
    Collection* collection;
    
    PlotArea(unsigned width, unsigned height);

};
