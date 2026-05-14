#include "PlotAreaTracker.hpp"


PlotAreaTracker::PlotAreaTracker() {}


void PlotAreaTracker::link(PlotArea* plot) {
  plots.emplace_back(plot);
}


void PlotAreaTracker::update() {
  for (PlotArea* plot : plots)
    plot->update();
}
