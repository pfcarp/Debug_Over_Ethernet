#include "RooflinePanel.hpp"


RooflinePanel::RooflinePanel() {
  parent = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  // Controls
  controls.scrollable = gtk_scrolled_window_new();
  controls.box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_widget_set_size_request(controls.scrollable, 300, -1);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(controls.scrollable), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_hexpand(controls.scrollable, FALSE);
  gtk_widget_set_vexpand(controls.scrollable, TRUE);
  //// Platform
  controls.platform.title = gtk_label_new("Board configuration");
  gtk_widget_add_css_class(controls.platform.title, "heading");
  gtk_box_append(GTK_BOX(controls.box), controls.platform.title);
  //// Selection box
  controls.platform.selection = gtk_combo_box_text_new();
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.selection), NULL, "Select a platform");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.selection), NULL, "UltraScale+");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.selection), NULL, "custom");
  gtk_combo_box_set_active(GTK_COMBO_BOX(controls.platform.selection), 0); // TODO cannot be selected back
  gtk_box_append(GTK_BOX(controls.box), controls.platform.selection);
  //// Events
  ////// Separator
  controls.platform.events.separator = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.events.separator);
  ////// Label
  controls.platform.events.label = gtk_label_new("Events mapping");
  gtk_widget_add_css_class(controls.platform.events.label, "heading");
  gtk_widget_set_halign(controls.platform.events.label, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.events.label);
  ////// Grid
  controls.platform.events.grid = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(controls.platform.events.grid), 6);
  gtk_grid_set_column_spacing(GTK_GRID(controls.platform.events.grid), 12);
  //////// Populate
  for (int i = 0; i < 2; i++) {
    ////////// Label
    controls.platform.events.entry[i].label = gtk_label_new((i == 0)? "Instructions:" : "LLC Refills:");
    gtk_widget_add_css_class(controls.platform.events.entry[i].label, "heading-2");
    gtk_widget_set_hexpand(controls.platform.events.entry[i].label, TRUE);
    gtk_widget_set_halign(controls.platform.events.entry[i].label, GTK_ALIGN_START);
    gtk_grid_attach(GTK_GRID(controls.platform.events.grid), controls.platform.events.entry[i].label, 0, i, 1, 1);
    ////////// Combo box
    controls.platform.events.entry[i].selection = gtk_combo_box_text_new();
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.events.entry[i].selection), NULL, "Event 0");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.events.entry[i].selection), NULL, "Event 1");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.events.entry[i].selection), NULL, "Event 2");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(controls.platform.events.entry[i].selection), NULL, "Event 3");
    gtk_combo_box_set_active(GTK_COMBO_BOX(controls.platform.events.entry[i].selection), i);
    gtk_widget_set_halign(controls.platform.events.entry[i].selection, GTK_ALIGN_CENTER);
    gtk_grid_attach(GTK_GRID(controls.platform.events.grid), controls.platform.events.entry[i].selection, 1, i, 1, 1);
    ////////// Scaler
    controls.platform.events.entry[i].scaler = gtk_spin_button_new_with_range(1, 1000000, 1);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(controls.platform.events.entry[i].scaler), TRUE);
    gtk_widget_set_halign(controls.platform.events.entry[i].scaler, GTK_ALIGN_CENTER);
    gtk_grid_attach(GTK_GRID(controls.platform.events.grid), controls.platform.events.entry[i].scaler, 2, i, 1, 1);
  }
  gtk_box_append(GTK_BOX(controls.box), controls.platform.events.grid);
  //// Delimitations
  ////// Separator
  controls.platform.delimitations.separator = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.separator);
  ////// Title
  controls.platform.delimitations.title = gtk_label_new("Delimitations");
  gtk_widget_add_css_class(controls.platform.delimitations.title, "heading");
  gtk_widget_set_halign(controls.platform.delimitations.title, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.title);
  ////// Performance
  //////// Box
  controls.platform.delimitations.performance.title.box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  //////// Label
  controls.platform.delimitations.performance.title.label = gtk_label_new("Performance:");
  gtk_widget_add_css_class(controls.platform.delimitations.performance.title.label, "heading-2");
  gtk_widget_set_halign(controls.platform.delimitations.performance.title.label, GTK_ALIGN_START);
  //////// Button
  controls.platform.delimitations.performance.title.button = gtk_button_new_from_icon_name("list-add-symbolic");
  GtkStyleContext* ctx1 = gtk_widget_get_style_context(GTK_WIDGET(controls.platform.delimitations.performance.title.button));
  gtk_style_context_add_class(ctx1, "suggested-action");
  gtk_widget_set_halign(controls.platform.delimitations.performance.title.button, GTK_ALIGN_END);
  g_signal_connect(controls.platform.delimitations.performance.title.button, "clicked", G_CALLBACK(RooflinePanel::cOnPerformanceButtonClicked), this);
  ////// Pack
  gtk_box_append(GTK_BOX(controls.platform.delimitations.performance.title.box), controls.platform.delimitations.performance.title.label);
  gtk_box_append(GTK_BOX(controls.platform.delimitations.performance.title.box), controls.platform.delimitations.performance.title.button);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.performance.title.box);
  ////// Entry
  controls.platform.delimitations.performance.box = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(controls.platform.delimitations.performance.box), 6);
  gtk_grid_set_column_spacing(GTK_GRID(controls.platform.delimitations.performance.box), 12);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.performance.box);
  controls.platform.delimitations.performance.colordialog.reserve(10);
  controls.platform.delimitations.performance.adjustment.reserve(10);
  controls.platform.delimitations.performance.entry.reserve(10);
  addPerformanceEntry();
  ////// Bandwidth
  //////// Box
  controls.platform.delimitations.bandwidth.title.box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  //////// Label
  controls.platform.delimitations.bandwidth.title.label = gtk_label_new("Bandwidth:");
  gtk_widget_add_css_class(controls.platform.delimitations.bandwidth.title.label, "heading-2");
  gtk_widget_set_halign(controls.platform.delimitations.bandwidth.title.label, GTK_ALIGN_START);
  //////// Button
  controls.platform.delimitations.bandwidth.title.button = gtk_button_new_from_icon_name("list-add-symbolic");
  GtkStyleContext* ctx0 = gtk_widget_get_style_context(GTK_WIDGET(controls.platform.delimitations.bandwidth.title.button));
  gtk_style_context_add_class(ctx0, "suggested-action");
  gtk_widget_set_halign(controls.platform.delimitations.bandwidth.title.button, GTK_ALIGN_END);
  g_signal_connect(controls.platform.delimitations.bandwidth.title.button, "clicked", G_CALLBACK(RooflinePanel::cOnBandwidthButtonClicked), this);
  ////// Pack
  gtk_box_append(GTK_BOX(controls.platform.delimitations.bandwidth.title.box), controls.platform.delimitations.bandwidth.title.label);
  gtk_box_append(GTK_BOX(controls.platform.delimitations.bandwidth.title.box), controls.platform.delimitations.bandwidth.title.button);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.bandwidth.title.box);
  ////// Entry
  controls.platform.delimitations.bandwidth.box = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(controls.platform.delimitations.bandwidth.box), 6);
  gtk_grid_set_column_spacing(GTK_GRID(controls.platform.delimitations.bandwidth.box), 12);
  gtk_box_append(GTK_BOX(controls.box), controls.platform.delimitations.bandwidth.box);
  controls.platform.delimitations.bandwidth.colordialog.reserve(10);
  controls.platform.delimitations.bandwidth.adjustment.reserve(10);
  controls.platform.delimitations.bandwidth.entry.reserve(10);
  addBandwidthEntry();
  // Plot
  plot = new PlotArea(0);
  // Pack
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(controls.scrollable), controls.box);
  gtk_box_append(GTK_BOX(parent), controls.scrollable);
  gtk_box_append(GTK_BOX(parent), plot->parent);
}


void RooflinePanel::addPerformanceEntry() {
  int row = controls.platform.delimitations.performance.visibility.size();
  // Visibility checkbox
  controls.platform.delimitations.performance.visibility.emplace_back(gtk_check_button_new());
  gtk_check_button_set_active(GTK_CHECK_BUTTON(controls.platform.delimitations.performance.visibility.back()), TRUE);
  //g_signal_connect(checkbox, "toggled", G_CALLBACK(PanelEntry::cOnCheckToggle), this);
  gtk_widget_set_halign(controls.platform.delimitations.performance.visibility.back(), GTK_ALIGN_CENTER);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.performance.box), controls.platform.delimitations.performance.visibility.back(), 0, row, 1, 1);
  // Color picker
  controls.platform.delimitations.performance.colordialog.emplace_back(gtk_color_dialog_new());
  controls.platform.delimitations.performance.color.emplace_back(gtk_color_dialog_button_new(controls.platform.delimitations.performance.colordialog.back()));
  gtk_widget_set_halign(controls.platform.delimitations.performance.color.back(), GTK_ALIGN_CENTER);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.performance.box), controls.platform.delimitations.performance.color.back(), 1, row, 1, 1);
  // Spin button
  controls.platform.delimitations.performance.adjustment.emplace_back(gtk_adjustment_new(0.0, 0.0, 10000000000.0, 1000000.0, 1.0, 0.0));
  controls.platform.delimitations.performance.entry.emplace_back(gtk_spin_button_new(controls.platform.delimitations.performance.adjustment.back(), 0.1, 2)); // climb rate, digits
  gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(controls.platform.delimitations.performance.entry.back()), TRUE);
  gtk_widget_set_hexpand(controls.platform.delimitations.performance.entry.back(), TRUE);
  gtk_widget_set_vexpand(controls.platform.delimitations.performance.entry.back(), FALSE);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.performance.box), controls.platform.delimitations.performance.entry.back(), 2, row, 1, 1);
  // Disbale button is limit is reached
  gtk_widget_set_sensitive(controls.platform.delimitations.performance.title.button, controls.platform.delimitations.performance.entry.size() < 10);
}


void RooflinePanel::addBandwidthEntry() {
  int row = controls.platform.delimitations.bandwidth.visibility.size();
  // Visibility checkbox
  controls.platform.delimitations.bandwidth.visibility.emplace_back(gtk_check_button_new());
  gtk_check_button_set_active(GTK_CHECK_BUTTON(controls.platform.delimitations.bandwidth.visibility.back()), TRUE);
  //g_signal_connect(checkbox, "toggled", G_CALLBACK(PanelEntry::cOnCheckToggle), this);
  gtk_widget_set_halign(controls.platform.delimitations.bandwidth.visibility.back(), GTK_ALIGN_CENTER);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.bandwidth.box), controls.platform.delimitations.bandwidth.visibility.back(), 0, row, 1, 1);
  // Color picker
  controls.platform.delimitations.bandwidth.colordialog.emplace_back(gtk_color_dialog_new());
  controls.platform.delimitations.bandwidth.color.emplace_back(gtk_color_dialog_button_new(controls.platform.delimitations.bandwidth.colordialog.back()));
  gtk_widget_set_halign(controls.platform.delimitations.bandwidth.color.back(), GTK_ALIGN_CENTER);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.bandwidth.box), controls.platform.delimitations.bandwidth.color.back(), 1, row, 1, 1);
  // Spin button
  controls.platform.delimitations.bandwidth.adjustment.emplace_back(gtk_adjustment_new(0.0, 0.0, 10000000000.0, 1000000.0, 1.0, 0.0));
  controls.platform.delimitations.bandwidth.entry.emplace_back(gtk_spin_button_new(controls.platform.delimitations.bandwidth.adjustment.back(), 0.1, 2)); // climb rate, digits
  gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(controls.platform.delimitations.bandwidth.entry.back()), TRUE);
  gtk_widget_set_hexpand(controls.platform.delimitations.bandwidth.entry.back(), TRUE);
  gtk_widget_set_vexpand(controls.platform.delimitations.bandwidth.entry.back(), FALSE);
  gtk_grid_attach(GTK_GRID(controls.platform.delimitations.bandwidth.box), controls.platform.delimitations.bandwidth.entry.back(), 2, row, 1, 1);
  // Disbale button is limit is reached
  gtk_widget_set_sensitive(controls.platform.delimitations.bandwidth.title.button, controls.platform.delimitations.bandwidth.entry.size() < 10);
}


void RooflinePanel::onPerformanceButtonClicked() {
  addPerformanceEntry();
}

void RooflinePanel::cOnPerformanceButtonClicked(GtkWidget* _, gpointer data) {
  RooflinePanel* self = static_cast<RooflinePanel*>(data);
  self->onPerformanceButtonClicked();
}


void RooflinePanel::onBandwidthButtonClicked() {
  addBandwidthEntry();
}

void RooflinePanel::cOnBandwidthButtonClicked(GtkWidget* _, gpointer data) {
  RooflinePanel* self = static_cast<RooflinePanel*>(data);
  self->onBandwidthButtonClicked();
}
