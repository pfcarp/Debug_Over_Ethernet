#include "SourcePanels.hpp"


#include "Packet.hpp"
#include "Description.hpp"
#include "TraceDatabase.hpp"
#include "PlotAreaTracker.hpp"


SourcePanels::SourcePanels() {
  parent = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  // Side panel
  side = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_box_append(GTK_BOX(parent), side);
  //// Controls
  ////// Setup scroll
  control.box = gtk_scrolled_window_new();
  ////// Setup grid
  control.grid = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(control.grid), 6);
  gtk_grid_set_column_spacing(GTK_GRID(control.grid), 12);
  ////// Set up header
  control.title.show = gtk_label_new("Show");
  gtk_widget_add_css_class(control.title.show, "heading");
  gtk_widget_set_hexpand(control.title.show, TRUE);
  gtk_grid_attach(GTK_GRID(control.grid), control.title.show, 0, 0, 1, 1);
  gtk_widget_set_halign(control.title.show, GTK_ALIGN_CENTER);
  control.title.color = gtk_label_new("Color");
  gtk_widget_add_css_class(control.title.color, "heading");
  gtk_widget_set_hexpand(control.title.color, TRUE);
  gtk_grid_attach(GTK_GRID(control.grid), control.title.color, 1, 0, 1, 1);
  gtk_widget_set_halign(control.title.color, GTK_ALIGN_CENTER);
  control.title.packet = gtk_label_new("Packet");
  gtk_widget_add_css_class(control.title.packet, "heading");
  gtk_widget_set_hexpand(control.title.packet, TRUE);
  gtk_grid_attach(GTK_GRID(control.grid), control.title.packet, 2, 0, 1, 1);
  gtk_widget_set_halign(control.title.packet, GTK_ALIGN_START);
  ////// Create entry for each packet
  control.entries.reserve(Packet::ColorMap.size());
  int row = 1;
  for (const auto& [name, color] : Packet::ColorMap) {
    control.entries.emplace_back(name, color, this);
    gtk_grid_attach(GTK_GRID(control.grid), control.entries.back().checkbox   , 0, row, 1, 1);
    gtk_grid_attach(GTK_GRID(control.grid), control.entries.back().colorpicker, 1, row, 1, 1);
    gtk_grid_attach(GTK_GRID(control.grid), control.entries.back().label      , 2, row, 1, 1);
    row++;
  }
  gtk_widget_set_hexpand(control.grid, FALSE);
  gtk_widget_set_vexpand(control.grid, TRUE);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(control.box), control.grid);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(control.box), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_box_append(GTK_BOX(side), control.box);
  //// Seperator 
  GtkWidget* separator = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(side), separator);
  //// Description
  description.box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
  gtk_box_append(GTK_BOX(side), description.box);
  ////// Title
  description.title = gtk_label_new("Description");
  gtk_widget_add_css_class(description.title, "heading");
  gtk_widget_set_halign(description.title, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(description.box), description.title);
  ////// Seperator 2
  description.separator = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(description.box), description.separator);
  ////// Text Box
  //////// Scrollable
  description.content.scrollable = gtk_scrolled_window_new();
  gtk_widget_set_vexpand(description.content.scrollable, TRUE);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(description.content.scrollable), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_box_append(GTK_BOX(description.box), description.content.scrollable);
  //////// Label
  description.content.label = gtk_label_new("");
  gtk_widget_set_vexpand(description.content.label, TRUE);
  gtk_widget_set_halign(description.content.label, GTK_ALIGN_START);
  gtk_widget_set_valign(description.content.label, GTK_ALIGN_START);
  gtk_label_set_wrap(GTK_LABEL(description.content.label), TRUE);
  gtk_label_set_wrap_mode(GTK_LABEL(description.content.label), PANGO_WRAP_WORD_CHAR);
  gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(description.content.scrollable), description.content.label);
  //////// Link
  Description::instance().link(description.content.label);
  //// Seperator
  GtkWidget* seperator = gtk_separator_new(GTK_ORIENTATION_VERTICAL);
  gtk_widget_set_vexpand(seperator, TRUE);
  gtk_box_append(GTK_BOX(parent), seperator);
  //// Panels
  panels.grid = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(panels.grid), 6);
  gtk_grid_set_column_spacing(GTK_GRID(panels.grid), 6);
  gtk_box_append(GTK_BOX(parent), panels.grid);
  TraceDatabase& database = TraceDatabase::instance();
  panels.widget.reserve(database.size());
  for (uint32_t i = 0; i < database.size(); i++) {
    panels.widget.emplace_back(i);
    int rows = static_cast<int>(std::sqrt(database.size()));
    int cols = (database.size()+rows-1)/rows;
    int row = i/rows;
    int col = i%cols;
    gtk_grid_attach(GTK_GRID(panels.grid), panels.widget.back().parent, col, row, 1, 1);
  }
}


PanelEntry::PanelEntry(const std::string& name, const Color& color, SourcePanels* parent): parent(parent) {
  // Show checkbox
  checkbox = gtk_check_button_new();
  gtk_check_button_set_active(GTK_CHECK_BUTTON(checkbox), (color.alpha > 0.0));
  g_signal_connect(checkbox, "toggled", G_CALLBACK(PanelEntry::cOnCheckToggle), this);
  gtk_widget_set_halign(checkbox, GTK_ALIGN_CENTER);
  // Color picker
  colorpickerDialog = gtk_color_dialog_new();
  colorpicker = gtk_color_dialog_button_new(colorpickerDialog);
  GdkRGBA initialColor = {color.red, color.green, color.blue, color.alpha};
  gtk_color_dialog_button_set_rgba(GTK_COLOR_DIALOG_BUTTON(colorpicker), &initialColor);
  g_signal_connect(colorpicker, "notify::rgba", G_CALLBACK(PanelEntry::cOnColorSet), this);
  gtk_widget_set_halign(colorpicker, GTK_ALIGN_CENTER);
  // Packet name label
  label = gtk_label_new(name.c_str());
  gtk_widget_set_halign(label, GTK_ALIGN_START);
}


void PanelEntry::onCheckToggle(GtkCheckButton* check) {
  const std::string name = std::string(gtk_label_get_text(GTK_LABEL(label)));
  Packet::ColorMap[name].alpha = gtk_check_button_get_active(check);
  PlotAreaTracker::instance().update();
}

void PanelEntry::cOnCheckToggle(GObject* object, gpointer user_data) {
  PanelEntry* self = static_cast<PanelEntry*>(user_data);
  self->onCheckToggle(GTK_CHECK_BUTTON(object));
}


void PanelEntry::onColorSet(GtkColorDialogButton* button) {
  const GdkRGBA* color = gtk_color_dialog_button_get_rgba(button);
  const std::string name = std::string(gtk_label_get_text(GTK_LABEL(label)));
  Packet::ColorMap[name].red   = color->red;
  Packet::ColorMap[name].blue  = color->blue;
  Packet::ColorMap[name].green = color->green;
  PlotAreaTracker::instance().update();
}


void PanelEntry::cOnColorSet(GObject* object, GParamSpec* pspec, gpointer user_data) {
  PanelEntry* self = static_cast<PanelEntry*>(user_data);
  self->onColorSet(GTK_COLOR_DIALOG_BUTTON(object));
}

