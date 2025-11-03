#include "Controls.hpp"


Controls::Controls(Buffer* buffer): buffer(buffer) {
  parent            = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
  // Label
  label             = gtk_label_new(buffer->event->name.c_str());
  gtk_box_append(GTK_BOX(parent), label);
  // HBOX
  buttons           = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_append(GTK_BOX(parent), buttons);
  //// Enabled
  checkbox          = gtk_check_button_new();
  gtk_check_button_set_active(GTK_CHECK_BUTTON(checkbox), buffer->show);
  g_signal_connect(checkbox, "toggled", G_CALLBACK(Controls::c_on_check_toggle), this);
  gtk_box_append(GTK_BOX(buttons), checkbox);
  //// Color picker
  colorpickerDialog = gtk_color_dialog_new();
  colorpicker       = gtk_color_dialog_button_new(colorpickerDialog);
  GdkRGBA initialColor = {buffer->event->color.red, buffer->event->color.green, buffer->event->color.blue, buffer->event->color.alpha};
  gtk_color_dialog_button_set_rgba(GTK_COLOR_DIALOG_BUTTON(colorpicker), &initialColor);
  g_signal_connect(colorpicker, "notify::rgba", G_CALLBACK(Controls::c_on_color_set), this);
  gtk_box_append(GTK_BOX(buttons), colorpicker);
}


void Controls::on_check_toggle(GtkCheckButton* check) {
  buffer->show = gtk_check_button_get_active(check);
}

void Controls::c_on_check_toggle(GObject* object, GParamSpec* pspec, gpointer user_data) {
  Controls* self = static_cast<Controls*>(user_data);
  self->on_check_toggle(GTK_CHECK_BUTTON(object));
}


void Controls::on_color_set(GtkColorDialogButton* button) {
  const GdkRGBA* color = gtk_color_dialog_button_get_rgba(button);
  buffer->event->color.red   = color->red;
  buffer->event->color.blue  = color->blue;
  buffer->event->color.green = color->green;
  buffer->event->color.alpha = color->alpha;
}


void Controls::c_on_color_set(GObject* object, GParamSpec* pspec, gpointer user_data) {
  Controls* self = static_cast<Controls*>(user_data);
  self->on_color_set(GTK_COLOR_DIALOG_BUTTON(object));
}

