#pragma once

#include <gtk/gtk.h>
#include <epoxy/gl.h> // For OpenGL functions

#include <vector>
#include <cmath>
#include <string>

#include "PlotAreaTracker.hpp"
#include "TraceDatabase.hpp"
#include "Packet.hpp"
#include "Color.hpp"

/**
 * Forward declaration
 */
class PlotAreaTracker;

class PlotArea {
  private:
    gboolean onScroll(double dy);
    static gboolean cOnScroll(GtkEventControllerScroll* controller, double dx, double dy, gpointer user_data);
    void onMotion(double x, double y);
    static void cOnMotion(GtkEventControllerMotion* controller, double x, double y, gpointer user_data);
    void onButtonPress(bool right, double x, double y);
    static void cOnButtonPress(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    void onButtonRelease();
    static void cOnButtonRelease(GtkGestureClick* gesture, int n_press, double x, double y, gpointer user_data);
    static void cOnDialogResponse(GtkDialog* _, int response_id, gpointer user_data);
    static void cOnDialogResponseUpdate(GtkDialog* _, int _1, gpointer user_data);
    static void cOnRealize(GtkGLArea* area, gpointer user_data);
    static void cOnRender(GtkGLArea* area, GdkGLContext* context, gpointer user_data);
    static void cOnUnrealize(GtkGLArea* area, gpointer user_data);
    void onRealize();
    void onRender(int width, int height);
    void onUnrealize();

    void plotScatterGL();
    void plotTimemarkersGL();
    void drawAxesGL();

    const double adaptX(const double& value, const double& min, const double& interval) const;
    const double adaptY(const double& value, const double& min, const double& interval) const;
    void clampOffset();
    std::vector<std::pair<uint64_t,uint32_t>> getPointsInRadius(uint64_t x, uint32_t y, double r);
    void rechargePointsBuffer();

    uint32_t id;
    PlotAreaTracker& tracker;
    const int nticks = 10;
    // Switch to identity matrix for screen-space rendering
    const float identity[16] = {
      1.0f, 0.0f, 0.0f, 0.0f,
      0.0f, 1.0f, 0.0f, 0.0f,
      0.0f, 0.0f, 1.0f, 0.0f,
      0.0f, 0.0f, 0.0f, 1.0f
    };
    std::vector<float> axes;
    std::vector<float> lines;
    std::vector<float> points;

    // OpenGL state
    GLuint shader_program;
    GLuint vbo_points; // For scatter points
    GLuint vao_points;
    GLuint vbo_lines;  // For timemarkers/axes
    GLuint vao_lines;
  
  public:
    PlotArea(uint32_t id, PlotAreaTracker& tracker);
    void update();
    GtkWidget* parent; // Now a GtkGLArea
};
