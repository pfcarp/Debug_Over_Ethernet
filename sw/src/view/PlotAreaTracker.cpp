#include "PlotAreaTracker.hpp"


PlotAreaTracker& PlotAreaTracker::instance() {
  static PlotAreaTracker instance;
  return instance;
}


void PlotAreaTracker::link(PlotArea* plot) {
  plots.emplace_back(plot);
}


void PlotAreaTracker::update() {
  for (PlotArea* plot : plots)
    plot->update();
}
