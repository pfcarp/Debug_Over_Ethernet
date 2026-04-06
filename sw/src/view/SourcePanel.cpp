#include "SourcePanel.hpp"


#include "PlotAreaTracker.hpp"


SourcePanel::SourcePanel(uint32_t sourceID, PlotAreaTracker& tracker): sourceID(sourceID), tracker(tracker) {
  parent = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  // header
  header.box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  header.title = gtk_label_new(("Source "+std::to_string(sourceID)).c_str());
  gtk_widget_add_css_class(header.title, "heading");
  gtk_widget_set_hexpand(header.title, TRUE);
  gtk_widget_set_halign(header.title, GTK_ALIGN_START);
  header.label = gtk_label_new("Cumulative?");
  gtk_widget_set_halign(header.label, GTK_ALIGN_END);
  header.switcher = gtk_switch_new();
  gtk_switch_set_active(GTK_SWITCH(header.switcher), TraceDatabase::instance()[sourceID].isCumulative());
  g_signal_connect(header.switcher, "notify::active", G_CALLBACK(SourcePanel::cOnCheckToggle), this);
  gtk_widget_set_halign(header.switcher, GTK_ALIGN_END);
  //// pack
  gtk_box_append(GTK_BOX(header.box), header.title);
  gtk_box_append(GTK_BOX(header.box), header.label);
  gtk_box_append(GTK_BOX(header.box), header.switcher);
  // plot
  plot = new PlotArea(sourceID, tracker);
  gtk_widget_queue_draw(plot->parent);
  // pack all
  gtk_box_append(GTK_BOX(parent), header.box);
  gtk_box_append(GTK_BOX(parent), plot->parent);
}

void SourcePanel::onCheckToggle(GtkSwitch* check) {
  TraceDatabase::instance()[sourceID].setCumulative(gtk_switch_get_active(check));
  tracker.update();
}

void SourcePanel::cOnCheckToggle(GObject* object, GParamSpec* pspec, gpointer user_data) {
  SourcePanel* self = static_cast<SourcePanel*>(user_data);
  self->onCheckToggle(GTK_SWITCH(object));
}

