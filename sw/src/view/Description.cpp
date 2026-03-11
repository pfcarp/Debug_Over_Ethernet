#include "Description.hpp"


Description& Description::instance() {
  static Description instance;
  return instance;
}

void Description::link(GtkWidget* target) {
  label = target;
}

void Description::reset() {
  content = "";
}

void Description::add(const std::string& msg) {
  content += msg;
}

void Description::update() {
  gtk_label_set_text(GTK_LABEL(label), content.c_str());
}
