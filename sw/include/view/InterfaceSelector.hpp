#pragma once


#include <gtk/gtk.h>


#include "Sniffer.hpp"


class InterfaceSelector {

  private:
    // Attributes
    Sniffer* sniffer = nullptr;
    // Methods
    void on_combo_changed(GtkComboBox* check);
    static void c_on_combo_changed(GObject* object, GParamSpec* pspec, gpointer user_data);

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    InterfaceSelector(Sniffer* sniffer);

};

