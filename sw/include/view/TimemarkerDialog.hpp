#pragma once


#include <gtk/gtk.h>


class TimemarkerDialog {

  private:
    // Atttributes
    GtkWidget* box;
    GtkWidget* header;
    GtkWidget* grid;
    GtkWidget* entry;
    GtkWidget* color;
    struct {
      GtkWidget* cancel;
      GtkWidget* confirm;
    } button;
    // Methods
    static void cOnNameChanged(GtkEditable* editable, gpointer user_data);
    static void cOnConfirmClicked(GtkButton* button, gpointer user_data);
    static void cOnCancelClicked(GtkButton* button, gpointer user_data);

  public:
    // Atttributes
    GtkWidget* parent;
    // Methods
    TimemarkerDialog(GtkWindow* window);
    void onDialogResponse(int response_id);

};
