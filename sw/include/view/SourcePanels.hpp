#pragma once


#include <gtk/gtk.h>
#include <cstdint>
#include <vector>


#include "PacketFactory.hpp"
#include "SourcePanel.hpp"
#include "Color.hpp"


// Foward declaration
class SourcePanels;


class PanelEntry {
    
  private:
    // Attributes
    SourcePanels* parent;
    GtkColorDialog* colorpickerDialog;

  public:
    // Attributes
    GtkWidget* checkbox;
    GtkWidget* colorpicker;
    GtkWidget* label;
    // Methods
    PanelEntry(const std::string& name, const Color& color, SourcePanels* parent);
    void onCheckToggle(GtkCheckButton* check);
    static void cOnCheckToggle(GObject* object, gpointer user_data);
    void onColorSet(GtkColorDialogButton* button);
    static void cOnColorSet(GObject* object, GParamSpec* pspec, gpointer user_data);

};


class SourcePanels {

  private:
    // Attributes
    static const uint32_t panelNumber = 1;
    SourcePanel* panel[panelNumber];
    GtkWidget* panels;
    struct {
      struct {
        GtkWidget* show;
        GtkWidget* color;
        GtkWidget* packet;
      } title;
      GtkWidget* box;
      GtkWidget* grid;
      std::vector<PanelEntry> entries;
    } control;
    // Methods

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    SourcePanels(std::vector<PacketFactory>& factories);
    void update();

};
