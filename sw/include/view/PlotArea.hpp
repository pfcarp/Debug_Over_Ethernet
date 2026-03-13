#pragma once


#include <cstdint>
#include <gtk/gtk.h>
#include <string>


#include "TraceDatabase.hpp"
#include "Packet.hpp"
#include "Color.hpp"


class PlotArea {

  private:
    // Attributes
    uint32_t id;
    cairo_t* cairo;
    // Methods
    const double adaptX(const double& value, const double& min, const double& interval) const;
    const double adaptY(const double& value, const double& min, const double& interval) const;
    std::vector<std::pair<uint64_t,uint32_t>> getPointsInRadius(uint64_t x, uint32_t y, double r);
    void onDraw(GtkDrawingArea *area, cairo_t* cr, int width, int height);
    static void cOnDraw(GtkDrawingArea* area, cairo_t* cr, int width, int height, gpointer user_data);
    gboolean onScroll(double dy);
    static gboolean cOnScroll(GtkEventControllerScroll* controller, double dx, double dy, gpointer user_data);
    void onMotion(double x, double y);
    static void cOnMotion(GtkEventControllerMotion* controller, double x, double y, gpointer user_data);
    void onButtonPress(bool right, double x, double y);
    static void cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    void onButtonRelease();
    static void cOnButtonRelease(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    static void cOnDialogResponse(GtkDialog* dialog, int response_id, gpointer user_data);
    void setBackground();
    void handleZoom();
    void clampOffset();
    void drawAxes();
    void plotTimemarkers();
    void drawTimemarkerHeaders();
    void plotCurve(const std::string& variant);
    void plotScatter(const std::string& variant);

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    PlotArea(uint32_t id);
    void update();

};
