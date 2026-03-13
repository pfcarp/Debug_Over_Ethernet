#pragma once


#include <gtk/gtk.h>
#include <cstdint>
#include <vector>


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
    struct {
      std::vector<SourcePanel> widget;
      GtkWidget* grid;
    } panels;
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
    struct {
      GtkWidget* box;
      GtkWidget* title;
      GtkWidget* separator;
      struct {
        GtkWidget* scrollable;
        GtkWidget* label;
      } content;
    } description;
    GtkWidget* side;
    // Methods

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    SourcePanels();
    void update();

};
