#pragma once


#include <gtk/gtk.h>


#include "Buffer.hpp"


class Controls {

  private:
    GtkWidget* buttons;
    Buffer* buffer;
    GtkWidget* label;
    GtkWidget* checkbox;
    GtkWidget* colorpicker;
    GtkColorDialog* colorpickerDialog;
    
    void on_check_toggle(GtkCheckButton* check);
    static void c_on_check_toggle(GObject* object, GParamSpec* pspec, gpointer user_data);
    void on_color_set(GtkColorDialogButton* button);
    static void c_on_color_set(GObject* object, GParamSpec* pspec, gpointer user_data);

  public:
    GtkWidget* parent;

    explicit Controls(Buffer* buffer);

};

