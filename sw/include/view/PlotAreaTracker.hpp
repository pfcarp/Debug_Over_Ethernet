#pragma once


#include <vector>


#include "PlotArea.hpp"


/**
 * Forward declaration
 */
class PlotArea;


// TODO: assumes that plots are all the same dimensions. Might not be the case for all.
class PlotAreaTracker {
  
  private:
    // Attributes
    std::vector<PlotArea*> plots;
    // Methods

  public:
    // Attributes
    struct {
      double x      = 50.0;
      double y      = 50.0;
      double width  =  0.0;
      double height =  0.0;
    } plot;
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
    } viewport;
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
    // Methods
    PlotAreaTracker();
    PlotAreaTracker(const PlotAreaTracker&) = delete;
    PlotAreaTracker& operator=(const PlotAreaTracker&) = delete;
    void link(PlotArea* plot);
    void update();

};
