#include <gtk/gtk.h>
#include <iostream>
#include <vector>
#include <mutex>


#include "Sniffer.hpp"
#include "Points.hpp"
#include "PlotArea.hpp"
#include "DataBuffer.hpp"
#include "HistogramBuffer.hpp"
#include "Controls.hpp"
#include "Inserter.hpp"
#include "Graph.hpp"
#include "GraphArea.hpp"
#include "InterfaceSelector.hpp"


// Model/data
static Sniffer* sniffer;
static Graph* graph = NULL;
static Collection* traces;
static Collection* watchpoints;
static Points* roofline;
static std::vector<Inserter*> inserters;

static std::vector<Event> eventsRoofline;
static std::vector<Event> eventsPerf;
// Overall control
static bool enabled = false;

// GTK widget
static PlotArea* plot;
static PlotArea* hist;
static PlotArea* roof;
static GraphArea* milestone;
static GtkWidget* button;
static GtkWidget* clearButton;
static std::vector<GtkWidget*> separators;
static std::vector<Controls*> controls;
static InterfaceSelector* interfacesSelector;


static void update_button() {
  GtkWidget *image;
  if (enabled)
    image = gtk_image_new_from_icon_name("media-playback-pause");
  else
    image = gtk_image_new_from_icon_name("media-playback-start");
  gtk_button_set_child(GTK_BUTTON(button), image);
}


static void on_click(GtkButton *button, gpointer user_data) {
  enabled = !enabled;
  if (enabled) {
    for (int i = 0; i < inserters.size(); i++) {
      inserters[i]->start();
    }
  }
  else {
    for (int i = 0; i < inserters.size(); i++) {
      inserters[i]->stop();
    }
  }
  update_button();
}


static void update_reset_button() {
  GtkWidget* image = gtk_image_new_from_icon_name("view-refresh");
  gtk_button_set_child(GTK_BUTTON(clearButton), image);
}


static gboolean update_plot(gpointer user_data) {
  if (enabled) {
    gtk_widget_queue_draw(plot->parent);
    gtk_widget_queue_draw(hist->parent);
    gtk_widget_queue_draw(roof->parent);
    gtk_widget_queue_draw(milestone->parent);
  }
  return G_SOURCE_CONTINUE;
}


static void on_reset_click(GtkButton* button, gpointer user_data) {
  for (int i = 0; i < traces->amount(); i++) {
    (*traces)[i]->clear();
  }
  for (int i = 0; i < watchpoints->amount(); i++) {
    (*watchpoints)[i]->clear();
  }
  update_reset_button();
  update_plot(NULL);
}


static void on_switch_toggled(GObject *gobject, GParamSpec *pspec, gpointer user_data) {
  for (int i = 0; i < watchpoints->amount(); i++) {
    (*watchpoints)[i]->cumulative = gtk_switch_get_active(GTK_SWITCH(gobject));
  }
  gtk_widget_queue_draw(hist->parent);
}


/**
 * Called once at the startup.
 */
static void on_activate(GtkApplication *app, gpointer user_data) {
  GtkWidget* window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "DoEth Live Tracer");
  gtk_window_set_default_size(GTK_WINDOW(window), 800, 200);
  
  // Header bar buttons
  GtkWidget* header_bar = gtk_header_bar_new();
  gtk_window_set_titlebar(GTK_WINDOW(window), header_bar);
  //// Start/Stop button
  button = gtk_button_new();
  update_button();
  g_signal_connect(button, "clicked", G_CALLBACK(on_click), NULL);
  gtk_header_bar_pack_start(GTK_HEADER_BAR(header_bar), button);
  //// Clear button
  clearButton = gtk_button_new();
  update_reset_button();
  g_signal_connect(clearButton, "clicked", G_CALLBACK(on_reset_click), NULL);
  gtk_header_bar_pack_start(GTK_HEADER_BAR(header_bar), clearButton);
  //// Interface selection
  interfacesSelector = new InterfaceSelector(sniffer);
  gtk_header_bar_pack_start(GTK_HEADER_BAR(header_bar), interfacesSelector->parent);

  GtkWidget* hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_window_set_child(GTK_WINDOW(window), hbox);

  milestone = new GraphArea(600, 600, graph);
  gtk_box_append(GTK_BOX(hbox), milestone->parent);

  GtkWidget* vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_append(GTK_BOX(hbox), vbox);

  // Trace drawing area
  plot = new PlotArea(800, 200);
  plot->collection = traces;
  gtk_box_append(GTK_BOX(vbox), plot->parent);
  // Bottom horizontal bar
  GtkWidget* traceControlBar = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_box_append(GTK_BOX(vbox), traceControlBar);
  //// Add all controls+DataBuffer
  for (int i = 0; i < traces->amount(); i++) {
    // Add separator
    separators.push_back(gtk_separator_new(GTK_ORIENTATION_VERTICAL));
    gtk_box_append(GTK_BOX(traceControlBar), separators.back());
    // Add control for DataBuffer
    controls.push_back(new Controls((*traces)[i]));
    gtk_box_append(GTK_BOX(traceControlBar), controls.back()->parent);
  }
  
  // Hist drawing area
  hist = new PlotArea(800, 200);
  hist->collection = watchpoints;
  gtk_box_append(GTK_BOX(vbox), hist->parent);
  // Bottom horizontal bar
  GtkWidget* histControlBar = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_box_append(GTK_BOX(vbox), histControlBar);
  // Bottom horizontal bar
  GtkWidget* cdfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_append(GTK_BOX(histControlBar), cdfBox);
  GtkWidget* cdfLabel = gtk_label_new("CDF?");
  gtk_box_append(GTK_BOX(cdfBox), cdfLabel);
  GtkWidget* cumulativeSwitch = gtk_switch_new();
  gtk_switch_set_active(GTK_SWITCH(cumulativeSwitch), FALSE);
  g_signal_connect(cumulativeSwitch, "notify::active", G_CALLBACK(on_switch_toggled), NULL);
  gtk_box_append(GTK_BOX(cdfBox), cumulativeSwitch);
  for (int i = 0; i < watchpoints->amount(); i++) {
    // Add separator
    separators.push_back(gtk_separator_new(GTK_ORIENTATION_VERTICAL));
    gtk_box_append(GTK_BOX(histControlBar), separators.back());
    // Add control for DataBuffer
    controls.push_back(new Controls((*watchpoints)[i]));
    gtk_box_append(GTK_BOX(histControlBar), controls.back()->parent);
  }

  // Roofline drawing area
  roof = new PlotArea(800, 200);
  roof->collection = roofline;
  gtk_box_append(GTK_BOX(vbox), roof->parent);
  
  // Refresh and all
  g_timeout_add(1000/60, update_plot, NULL);
  gtk_window_present(GTK_WINDOW(window));
}


int main(int argc, char *argv[]) {
  GtkApplication *app;
  int status;

  eventsRoofline = {Event("Roofline")};
  eventsPerf = {Event("Inst. retired"), Event("L1 refills"), Event("L2 refills"), Event("DTLB refills")};

  sniffer = new Sniffer();

  graph = new Graph("inputs/toy_example.dot");
  traces = new Collection();
  for (int i = 0; i < eventsPerf.size(); i++) {
    traces->add(new DataBuffer(&eventsPerf[i]));
    inserters.push_back(new InserterLinear((*traces)[i], i*1.57));
  }
  watchpoints = new Collection();
  for (int i = 0; i < graph->nodes.size(); i++) {
    watchpoints->add(new HistogramBuffer(&graph->nodes[i]));
    inserters.push_back(new InserterNormal((*watchpoints)[i], (i+1)*25));
  }
  roofline = new Points(&eventsRoofline[0]);
  inserters.push_back(new InserterStep((*roofline)[0], 25));
  // Generate roofline
  app = gtk_application_new("com.example.LivePlot", G_APPLICATION_DEFAULT_FLAGS);

  g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);
  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  for (size_t i = 0; i < controls.size(); i++)
    delete controls[i];
  for (size_t i = 0; i < traces->amount(); i++)
    delete (*traces)[i];
  for (size_t i = 0; i < watchpoints->amount(); i++)
    delete (*watchpoints)[i];
  for (size_t i = 0; i < inserters.size(); i++)
    delete inserters[i];
  delete traces;
  delete watchpoints;
  delete plot;
  delete graph;
  delete interfacesSelector;
  delete sniffer;

  return status;
}

