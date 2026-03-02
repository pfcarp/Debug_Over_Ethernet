#pragma once


#include <gtk/gtk.h>


#include "PacketFactory.hpp"
#include "Trace.hpp"
#include "Packet.hpp"
#include "Color.hpp"


class PlotArea {

  private:
    // Attributes
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
    PacketFactory* factory;
    // Methods
    inline double plotWidth();
    inline double plotHeight();
    inline double adaptX(double value);
    inline double adaptY(double value);
    void onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height);
    static void cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data);
    void setBackground();
    void drawAxes();
    void plotCurve(const Color* color, const Trace* buffer);
    void plotScatter(const Color* color, const Trace* buffer);

  public:
    GtkWidget* parent;
    
    PlotArea(unsigned width, unsigned height, PacketFactory* factory);

};
