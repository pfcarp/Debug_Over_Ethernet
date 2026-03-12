#pragma once


#include <gtk/gtk.h>
#include <cstdint>


class TimemarkerDialog {

  private:
    // Atttributes
    GtkWidget* box;
    GtkWidget* header;
    GtkWidget* grid;
    GtkWidget* entry;
    struct {
      GtkColorDialog* dialog;
      GtkWidget* picker;
    } color;
    struct {
      GtkWidget* cancel;
      GtkWidget* confirm;
    } button;
    uint64_t timestamp;
    // Methods
    static void cOnNameChanged(GtkEditable* editable, gpointer user_data);
    static void cOnConfirmClicked(GtkButton* button, gpointer user_data);
    static void cOnCancelClicked(GtkButton* button, gpointer user_data);

  public:
    // Atttributes
    GtkWidget* parent;
    // Methods
    TimemarkerDialog(GtkWindow* window, uint64_t timestamp);
    void onDialogResponse(int response_id);

};
