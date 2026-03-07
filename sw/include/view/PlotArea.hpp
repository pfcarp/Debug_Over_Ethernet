#pragma once


#include <gtk/gtk.h>
#include <string>


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
    struct {
      double scale = 1.0;
      struct {
        double x = 0.0;
        double y = 0.0;
      } offset;
    } zoom;
    struct {
      double width;
      double height;
    } world;
    struct {
      struct {
        double x = 0.0;
        double y = 0.0;
      } current;
      struct {
        double x = 0.0;
        double y = 0.0;
      } last;
      bool dragging = false;
    } mouse;
    cairo_t* cairo;
    PacketFactory& factory;
    // Methods
    inline double plotWidth();
    inline double plotHeight();
    inline double adaptX(double value);
    inline double adaptY(double value);
    void onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height);
    static void cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data);
    gboolean onScroll(double dy);
    static gboolean cOnScroll(GtkEventControllerScroll* controller, double dx, double dy, gpointer user_data);
    void onMotion(double x, double y);
    static void cOnMotion(GtkEventControllerMotion* controller, double x, double y, gpointer user_data);
    void onButtonPress(double x, double y);
    static void cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    void onButtonRelease();
    static void cOnButtonRelease(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    void setBackground();
    void handleZoom();
    void clampOffset();
    void drawAxes();
    void plotCurve(const std::string& variant);
    void plotScatter(const std::string& variant);

  public:
    GtkWidget* parent;
    
    PlotArea(unsigned width, unsigned height, PacketFactory& factory);

};
