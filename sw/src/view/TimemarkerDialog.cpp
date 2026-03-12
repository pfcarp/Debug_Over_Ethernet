#include "TimemarkerDialog.hpp"


TimemarkerDialog::TimemarkerDialog(GtkWindow* window) {
  parent = gtk_dialog_new();
  gtk_window_set_transient_for(GTK_WINDOW(parent), window);
  gtk_window_set_modal(GTK_WINDOW(parent), TRUE);

  header = gtk_header_bar_new();
  gtk_window_set_titlebar(GTK_WINDOW(parent), header);
  gtk_header_bar_set_title_widget(GTK_HEADER_BAR(header), gtk_label_new("Create timemarker"));
  gtk_header_bar_set_show_title_buttons(GTK_HEADER_BAR(header), FALSE);

  box = gtk_dialog_get_content_area(GTK_DIALOG(parent));

  grid = gtk_grid_new();
  gtk_grid_set_row_spacing(GTK_GRID(grid), 10);
  gtk_grid_set_column_spacing(GTK_GRID(grid), 10);
  gtk_widget_set_margin_top(grid, 12);
  gtk_widget_set_margin_bottom(grid, 12);
  gtk_widget_set_margin_start(grid, 12);
  gtk_widget_set_margin_end(grid, 12);
  gtk_widget_set_halign(grid, GTK_ALIGN_CENTER);
  gtk_widget_set_valign(grid, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(box), grid);

  // Content
  entry = gtk_entry_new();
  gtk_grid_attach(GTK_GRID(grid), entry, 0, 0, 1, 1);
  gtk_entry_set_placeholder_text(GTK_ENTRY(entry), "Enter name");
  color = gtk_color_dialog_button_new(NULL);
  gtk_grid_attach(GTK_GRID(grid), color, 1, 0, 1, 1);

  // Buttons
  button.cancel = gtk_button_new_with_label("Cancel");
  gtk_widget_add_css_class(button.cancel, "destructive-action");
  gtk_header_bar_pack_start(GTK_HEADER_BAR(header), button.cancel);
  button.confirm = gtk_button_new_with_label("Confirm");
  gtk_widget_add_css_class(button.confirm, "suggested-action");
  gtk_widget_set_sensitive(button.confirm, FALSE);
  gtk_header_bar_pack_end(GTK_HEADER_BAR(header), button.confirm);

  g_signal_connect(entry, "changed", G_CALLBACK(TimemarkerDialog::cOnNameChanged), button.confirm);
  g_signal_connect(button.cancel, "clicked", G_CALLBACK(TimemarkerDialog::cOnCancelClicked), this);
  g_signal_connect(button.confirm, "clicked", G_CALLBACK(TimemarkerDialog::cOnConfirmClicked), this);
  // Bind enter key to confirm
  gtk_widget_set_receives_default(button.confirm, TRUE);
  gtk_window_set_default_widget(GTK_WINDOW(parent), button.confirm);
}


void TimemarkerDialog::cOnNameChanged(GtkEditable* editable, gpointer user_data) {
  GtkWidget *confirm = GTK_WIDGET(user_data);
  const char *text = gtk_editable_get_text(editable);
  gtk_widget_set_sensitive(confirm, text && *text);
}


void TimemarkerDialog::cOnConfirmClicked(GtkButton* button, gpointer user_data) {
  const TimemarkerDialog* dialog = static_cast<TimemarkerDialog*>(user_data);
  gtk_dialog_response(GTK_DIALOG(dialog->parent), GTK_RESPONSE_ACCEPT);
}


void TimemarkerDialog::cOnCancelClicked(GtkButton* button, gpointer user_data) {
  const TimemarkerDialog* dialog = static_cast<TimemarkerDialog*>(user_data);
  gtk_dialog_response(GTK_DIALOG(dialog->parent), GTK_RESPONSE_CANCEL);
}

void TimemarkerDialog::onDialogResponse(int response_id) {
  if (response_id == GTK_RESPONSE_ACCEPT) {
    const char* name = gtk_editable_get_text(GTK_EDITABLE(entry));
    const GdkRGBA* colorSelection = gtk_color_dialog_button_get_rgba(GTK_COLOR_DIALOG_BUTTON(color));
    g_print("Create marker: %s\n", name);
    /* create_timemarker(name, &color); */
  }
  gtk_window_destroy(GTK_WINDOW(parent));
}
