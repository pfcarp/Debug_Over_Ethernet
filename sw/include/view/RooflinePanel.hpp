#pragma once


#include <gtk/gtk.h>
#include <vector>


#include "PlotArea.hpp"


class RooflinePanel {

  private:
    // Attributes
    struct {
      GtkWidget* scrollable;
      GtkWidget* box;
      struct {
        GtkWidget* title;
        GtkWidget* selection;
        struct {
          GtkWidget* separator;
          GtkWidget* label;
          GtkWidget* grid;
          struct {
            GtkWidget* label;
            GtkWidget* selection;
            GtkWidget* scaler;
          } entry[2];
        } events;
        struct {
          GtkWidget* separator;
          GtkWidget* title;
          struct {
            struct {
              GtkWidget* box;
              GtkWidget* label;
              GtkWidget* button;
            } title;
            GtkWidget* box;
            std::vector<GtkWidget*> visibility;
            std::vector<GtkColorDialog*> colordialog;
            std::vector<GtkWidget*> color;
            std::vector<GtkAdjustment*> adjustment;
            std::vector<GtkWidget*> entry;
          } performance;
          struct {
            struct {
              GtkWidget* box;
              GtkWidget* label;
              GtkWidget* event;
              GtkWidget* button;
            } title;
            GtkWidget* box;
            std::vector<GtkWidget*> visibility;
            std::vector<GtkColorDialog*> colordialog;
            std::vector<GtkWidget*> color;
            std::vector<GtkAdjustment*> adjustment;
            std::vector<GtkWidget*> entry;
          } bandwidth;
        } delimitations;
      } platform;
    } controls;
    PlotArea* plot;
    // Methods
    void addPerformanceEntry();
    static void cOnPerformanceButtonClicked(GtkWidget* _, gpointer data);
    void onPerformanceButtonClicked();
    void addBandwidthEntry();
    static void cOnBandwidthButtonClicked(GtkWidget* _, gpointer data);
    void onBandwidthButtonClicked();

  public:
    // Attributes
    GtkWidget* parent;
    // Methods
    RooflinePanel();

};
