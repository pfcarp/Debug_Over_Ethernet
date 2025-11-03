#include "Collection.hpp"


#include <cmath>


double Collection::xmin() {
  double xmin = INFINITY;
  for (int i = 0; i < buffers.size(); i++) {
    double tmp = buffers[i]->xmin();
    if (tmp < xmin)
       xmin = tmp;
  }
  return xmin;
}

double Collection::xmax() {
  double xmax = -INFINITY;
  for (int i = 0; i < buffers.size(); i++) {
    double tmp = buffers[i]->xmax();
    if (tmp > xmax)
       xmax = tmp;
  }
  return xmax;
}

double Collection::ymin() {
  double ymin = INFINITY;
  for (int i = 0; i < buffers.size(); i++) {
    double tmp = buffers[i]->ymin();
    if (tmp < ymin)
       ymin = tmp;
  }
  return ymin;
}

double Collection::ymax() {
  double ymax = -INFINITY;
  for (int i = 0; i < buffers.size(); i++) {
    double tmp = buffers[i]->ymax();
    if (tmp > ymax)
      ymax = tmp;
  }
  return ymax;
}


void Collection::add(Buffer* buffer) {
  buffers.push_back(buffer);
}


unsigned Collection::amount() {
  return buffers.size();
}


Buffer* Collection::operator[](size_t index) {
  return buffers[index];
}


void Collection::clear() {
  for (size_t i = 0; i < buffers.size(); i++) {
    buffers[i]->clear();
  }
}
