#pragma once


#include <gtk/gtk.h>
#include <cstdint>


#include "PlotArea.hpp"
#include "PacketFactory.hpp"


class SourcePanel {

  private:
    // Attributes
    PacketFactory* factory;
    struct {
      GtkWidget* box;
      GtkWidget* title;
      GtkWidget* label;
      GtkWidget* switcher;
    } header;
    PlotArea* plot;
    // Methods
    void onCheckToggle(GtkSwitch* check);
    static void cOnCheckToggle(GObject* object, GParamSpec* pspec, gpointer user_data);

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    SourcePanel(uint32_t sourceID, PacketFactory* factory);
    void update();

};
