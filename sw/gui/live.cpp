#include <cstdint>
#include <gtk/gtk.h>
#include <ostream>
#include <vector>
#include <string>
#include <map>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <chrono>


#include "Sniffer.hpp"
#include "SourcePanels.hpp"
#include "RooflinePanel.hpp"
#include "Deformatter.hpp"
#include "PlotArea.hpp"


SourcePanels* panels;
RooflinePanel* roofline;


class Step {
  
  private:
    std::string name;
    GtkWidget* hbox = nullptr;
    GtkWidget* name_label = nullptr;
    GtkWidget* check_label = nullptr;
  
  public:
    GtkWidget* row = nullptr;
    Step(std::string name);
    void markAsVisited();
    std::string getName();

};

Step::Step(std::string name): name(name) {
  // Setup row
  row = gtk_list_box_row_new();
  gtk_widget_set_name(row, name.c_str());
  //// Create entry
  hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
  ////// Labels
  name_label = gtk_label_new(name.c_str());
  check_label = gtk_label_new("");
  ////// Place labels
  gtk_widget_set_halign(name_label, GTK_ALIGN_START);
  gtk_widget_set_hexpand(name_label, TRUE);
  gtk_widget_set_halign(check_label, GTK_ALIGN_END);
  ////// Pack labels
  gtk_box_append(GTK_BOX(hbox), name_label);
  gtk_box_append(GTK_BOX(hbox), check_label);
  //// Pack
  gtk_list_box_row_set_child(GTK_LIST_BOX_ROW(row), hbox);
}

void Step::markAsVisited() {
  gtk_label_set_text(GTK_LABEL(check_label), "✅");
}

std::string Step::getName() {
  return name;
}


class Page {

  protected:
    std::string name;

  public:
    GtkWidget* page = nullptr;
    Step step;
    Page(std::string name);
    virtual void onActivate(GtkWidget* widget);
    std::string getName();

};

Page::Page(std::string name): name(name), step(Step(name)) {}

void Page::onActivate(GtkWidget* widget) {}

std::string Page::getName() {
  return name;
}


class StartPage: public Page {

  private:

  public:
    StartPage();

};

StartPage::StartPage(): Page("Start guide") {
  page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 20);
  // Center children inside the box
  gtk_widget_set_halign(page, GTK_ALIGN_CENTER);
  gtk_widget_set_valign(page, GTK_ALIGN_CENTER);
  // Title
  GtkWidget *title = gtk_label_new("Welcome to the DoEth live tracer!");
  gtk_widget_add_css_class(title, "title-1");  // Large GTK theme title
  gtk_label_set_justify(GTK_LABEL(title), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(title, GTK_ALIGN_CENTER);
  // Description
  GtkWidget *desc = gtk_label_new("The next step will guide you through the setup. On the left, you will find the three necessary steps. Proceed from one to the next by clicking the 'next' button on the top right. Note that, in the next step, you will have the possibility to select whether you want to load a trace from a file or from your ethernet connection.");
  gtk_label_set_wrap(GTK_LABEL(desc), TRUE);
  gtk_label_set_justify(GTK_LABEL(desc), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(desc, GTK_ALIGN_CENTER);
  // Append to box
  gtk_box_append(GTK_BOX(page), title);
  gtk_box_append(GTK_BOX(page), desc);
}


class AcquireTracePage: public Page {

  private:
    // Attributes
    GtkWidget* stack;
    GtkWidget* switcher;
    struct {
      GtkWidget* page;
      GtkWidget* row;
      GtkWidget* entry;
      GtkWidget* title;
      GtkWidget* desc;
      GtkWidget* browse;
    } file;
    struct {
      GtkWidget* page;
      GtkWidget* title[2];
      GtkWidget* desc[2];
      GtkWidget* row[3];
      GtkWidget* combo;
      GtkWidget* play;
      GtkWidget* stop;
      GtkWidget* buffer_size_label;
      GtkWidget* spinner;
      guint      timeout_id = 0;
      GtkWidget* entry;
      GtkWidget* browse;
      GtkWidget* check;
    } ethernet;
    std::vector<uint8_t>* buffer;
    Sniffer* sniffer;
    // Methods
    std::string getCurrentTimestamp();
    static void cOnFileBrowseClicked(GtkButton *btn, gpointer user_data);
    static void cOnFolderBrowseClicked(GtkButton *btn, gpointer user_data);
    static void cOnFileSelected(GObject *source, GAsyncResult *res, gpointer data);
    static void cOnFolderSelected(GObject *source, GAsyncResult *res, gpointer data);
    static void cOnPlayClicked(GtkWidget* _, gpointer data);
    static void cOnStopClicked(GtkWidget* _, gpointer data);
    static void cOnCheckToggle(GtkWidget* _, gpointer data);
    static gboolean cUpdateLabel(gpointer data);
    void onPlayClicked();
    void onStopClicked();
    void onCheckToggle();
    gboolean updateLabel();

  public:
    AcquireTracePage(std::vector<uint8_t>* buffer, Sniffer* sniffer);
    void onActivate(GtkWidget* _);

};

std::string AcquireTracePage::getCurrentTimestamp() {
  // Get current time
  auto now = std::chrono::system_clock::now();
  std::time_t now_time = std::chrono::system_clock::to_time_t(now);
  // Convert to local time
  std::tm local_time = *std::localtime(&now_time);
  // Format as a string (e.g., "2026-05-02-14-30-45")
  std::ostringstream oss;
  oss << std::put_time(&local_time, "%Y-%m-%d-%H-%M-%S");
  return oss.str();
}

void AcquireTracePage::cOnFileBrowseClicked(GtkButton *btn, gpointer user_data) {
  GtkWidget* entry = GTK_WIDGET(user_data);
  GtkFileDialog* dialog = gtk_file_dialog_new();
  gtk_file_dialog_set_title(dialog, "Select a file");
  gtk_file_dialog_set_modal(dialog, TRUE);
  gtk_file_dialog_open(dialog, NULL, NULL, cOnFileSelected, entry);
}

void AcquireTracePage::cOnFileSelected(GObject *source, GAsyncResult *res, gpointer data) {
  GtkWidget* entry = GTK_WIDGET(data);
  GFile* file = gtk_file_dialog_open_finish(GTK_FILE_DIALOG(source), res, NULL);
  if (file) {
    char* path = g_file_get_path(file);
    gtk_editable_set_text(GTK_EDITABLE(entry), path);
    g_free(path);
    g_object_unref(file);
  }
}

void AcquireTracePage::cOnFolderBrowseClicked(GtkButton *btn, gpointer user_data) {
  GtkWidget* entry = GTK_WIDGET(user_data);
  GtkFileDialog* dialog = gtk_file_dialog_new();
  gtk_file_dialog_set_title(dialog, "Select a folder");
  gtk_file_dialog_set_modal(dialog, TRUE);
  gtk_file_dialog_select_folder(dialog, NULL, NULL, cOnFolderSelected, entry);
}

void AcquireTracePage::cOnFolderSelected(GObject *source, GAsyncResult *res, gpointer data) {
  GtkWidget* entry = GTK_WIDGET(data);
  GFile* folder = gtk_file_dialog_select_folder_finish(GTK_FILE_DIALOG(source), res, NULL);
  if (folder) {
    char* path = g_file_get_path(folder);
    gtk_editable_set_text(GTK_EDITABLE(entry), path);
    g_free(path);
    g_object_unref(folder);
  }
}

void AcquireTracePage::cOnPlayClicked(GtkWidget* _, gpointer user_data) {
  AcquireTracePage* self = static_cast<AcquireTracePage*>(user_data);
  self->onPlayClicked();
}

void AcquireTracePage::cOnStopClicked(GtkWidget* _, gpointer user_data) {
  AcquireTracePage* self = static_cast<AcquireTracePage*>(user_data);
  self->onStopClicked();
}

void AcquireTracePage::cOnCheckToggle(GtkWidget* _, gpointer user_data) {
  AcquireTracePage* self = static_cast<AcquireTracePage*>(user_data);
  self->onCheckToggle();
}

gboolean AcquireTracePage::cUpdateLabel(gpointer user_data) {
  AcquireTracePage* self = static_cast<AcquireTracePage*>(user_data);
  return self->updateLabel();
}

void AcquireTracePage::onPlayClicked() {
  const gchar* selectedText = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(ethernet.combo));
  if (selectedText != NULL) {
    std::string selection(selectedText);
    g_free((gpointer)selectedText);
    // disable actions
    gtk_widget_set_sensitive(GTK_WIDGET(ethernet.combo), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(ethernet.play) , FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(ethernet.stop) , TRUE );
    // pick device selection
    sniffer->pickDevice(selection);
    //// Start update thread
    gtk_spinner_start(GTK_SPINNER(ethernet.spinner));
    gtk_widget_set_visible(ethernet.spinner, TRUE);
    ethernet.timeout_id = g_timeout_add(100, (GSourceFunc)cUpdateLabel, this);
  }
}

void AcquireTracePage::onStopClicked() {
  // Unpick device
  sniffer->unpickDevice();
  // Enable actions
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.combo), TRUE );
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.play) , TRUE );
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.stop) , FALSE);
  // Stop update thread
  gtk_spinner_stop(GTK_SPINNER(ethernet.spinner));
  gtk_widget_set_visible(ethernet.spinner, FALSE);
  if (ethernet.timeout_id != 0) {
    g_source_remove(ethernet.timeout_id);
    ethernet.timeout_id = 0;
  }
}

void AcquireTracePage::onCheckToggle() {
  bool active = gtk_check_button_get_active(GTK_CHECK_BUTTON(ethernet.check));
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.entry) , active);
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.browse), active);
}

gboolean AcquireTracePage::updateLabel() {
  gchar* text = g_strdup_printf("Data captured: %lu Bytes", buffer->size());
  gtk_label_set_text(GTK_LABEL(ethernet.buffer_size_label), text);
  g_free(text);
  return G_SOURCE_CONTINUE; // Keep the timeout running
}

AcquireTracePage::AcquireTracePage(std::vector<uint8_t>* buffer, Sniffer* sniffer): Page("Acquire Trace"), buffer(buffer), sniffer(sniffer) {
  page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  // Stack container
  stack = gtk_stack_new();
  gtk_widget_set_vexpand(stack, TRUE);
  gtk_widget_set_hexpand(stack, TRUE);

  // Switcher (tabs header)
  switcher = gtk_stack_switcher_new();
  gtk_stack_switcher_set_stack(GTK_STACK_SWITCHER(switcher), GTK_STACK(stack));

  //// File Page
  file.page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
  /* Horizontal row for entry + button */
  file.row = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  file.entry = gtk_entry_new();
  gtk_widget_set_hexpand(file.entry, TRUE);
  //// Interface label
  file.title = gtk_label_new("File selection.");
  gtk_widget_add_css_class(file.title, "heading");
  // Description
  file.desc = gtk_label_new("Select the source trace file of interrest by clicking the 'Browse' button. Make sure it is correct.");
  gtk_label_set_wrap(GTK_LABEL(file.desc), TRUE);
  gtk_label_set_justify(GTK_LABEL(file.desc), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(file.desc, GTK_ALIGN_CENTER);
  ////// Create browse button
  file.browse = gtk_button_new_with_label("Browse");
  ////// Store entry pointer inside button for callback access
  g_object_set_data(G_OBJECT(file.browse), "entry", file.entry);
  ////// Browse callback
  g_signal_connect(file.browse, "clicked", G_CALLBACK(cOnFileBrowseClicked), file.entry);
  //// Assemble row
  gtk_box_append(GTK_BOX(file.row), file.entry);
  gtk_box_append(GTK_BOX(file.row), file.browse);
  //// Add to page
  gtk_box_append(GTK_BOX(file.page), file.title);
  gtk_box_append(GTK_BOX(file.page), file.desc);
  gtk_box_append(GTK_BOX(file.page), file.row);
  // Add to stack
  gtk_stack_add_titled(GTK_STACK(stack), file.page, "file", "Load from file");

  // Ethernet Page
  ethernet.page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
  gtk_stack_add_titled(GTK_STACK(stack), ethernet.page, "ethernet", "Load over Ethernet");
  //// Interface label
  ethernet.title[0] = gtk_label_new("Ethernet interface selection.");
  gtk_widget_add_css_class(ethernet.title[0], "heading");
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.title[0]);
  // Description
  ethernet.desc[0] = gtk_label_new("Select the interface to listen to to get the trace.");
  gtk_label_set_wrap(GTK_LABEL(ethernet.desc[0]), TRUE);
  gtk_label_set_justify(GTK_LABEL(ethernet.desc[0]), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(ethernet.desc[0], GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.desc[0]);
  //// TOP ROW : Interface + Controls
  ethernet.row[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  ////// Combo box text
  ethernet.combo = gtk_combo_box_text_new();
  for (const auto& device : sniffer->getDevices()) {
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(ethernet.combo), NULL, device.c_str());
  }
  gtk_widget_set_hexpand(ethernet.combo, TRUE);
  gtk_box_append(GTK_BOX(ethernet.row[0]), ethernet.combo);
  ////// Play button
  ethernet.play = gtk_button_new_from_icon_name("media-playback-start");
  g_signal_connect(ethernet.play, "clicked", G_CALLBACK(cOnPlayClicked), this);
  gtk_box_append(GTK_BOX(ethernet.row[0]), ethernet.play);
  ////// Stop button
  ethernet.stop = gtk_button_new_from_icon_name("media-playback-stop");
  g_signal_connect(ethernet.stop, "clicked", G_CALLBACK(cOnStopClicked), this);
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.stop) , FALSE);
  gtk_box_append(GTK_BOX(ethernet.row[0]), ethernet.stop);
  ////// Push top row into main box
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.row[0]);
  ////// 
  ethernet.row[1] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  gtk_widget_set_halign(ethernet.row[1], GTK_ALIGN_CENTER);
  //////// Label
  ethernet.buffer_size_label = gtk_label_new("Data captured: 0 Bytes");
  gtk_box_append(GTK_BOX(ethernet.row[1]), ethernet.buffer_size_label);
  //////// Spinner
  ethernet.spinner = gtk_spinner_new();
  gtk_widget_set_visible(ethernet.spinner, FALSE);
  gtk_box_append(GTK_BOX(ethernet.row[1]), ethernet.spinner);
  ////// Push top row into main box
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.row[1]);
  //// TRACE SAVING SECTION
  ////// Title label
  ethernet.title[1] = gtk_label_new("Save trace?");
  gtk_widget_add_css_class(ethernet.title[1], "heading");
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.title[1]);
  ////// Description
  ethernet.desc[1] = gtk_label_new("Select a destination (folder) to store the recoding of the trace by clicking the 'Browse' button. Confirm the action by checking the box below.");
  gtk_label_set_wrap(GTK_LABEL(ethernet.desc[1]), TRUE);
  gtk_label_set_justify(GTK_LABEL(ethernet.desc[1]), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(ethernet.desc[1], GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.desc[1]);
  ////// File picker row
  ethernet.row[2] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  ////// Checkbox
  ethernet.check = gtk_check_button_new();
  g_signal_connect(ethernet.check, "toggled", G_CALLBACK(cOnCheckToggle), this);
  gtk_widget_set_halign(ethernet.check, GTK_ALIGN_CENTER);
  gtk_widget_set_margin_start(ethernet.check, 10); // Left margin
  gtk_widget_set_margin_end(ethernet.check, 10);   // Right margin
  ////// Entry
  ethernet.entry = gtk_entry_new();
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.entry) , FALSE);
  gtk_widget_set_hexpand(ethernet.entry, TRUE);
  ethernet.browse = gtk_button_new_with_label("Browse");
  gtk_widget_set_sensitive(GTK_WIDGET(ethernet.browse), FALSE);
  ////// Connect browse button (reuse your previous pattern) */
  g_signal_connect(ethernet.browse, "clicked", G_CALLBACK(cOnFolderBrowseClicked), ethernet.entry);
  // pack all in row 2
  gtk_box_append(GTK_BOX(ethernet.row[2]), ethernet.check);
  gtk_box_append(GTK_BOX(ethernet.row[2]), ethernet.entry);
  gtk_box_append(GTK_BOX(ethernet.row[2]), ethernet.browse);
  gtk_box_append(GTK_BOX(ethernet.page), ethernet.row[2]);
  // Add switcher + stack to page
  gtk_box_append(GTK_BOX(page), switcher);
  gtk_box_append(GTK_BOX(page), stack);
}

void AcquireTracePage::onActivate(GtkWidget* _) {
  GtkWidget* visible = gtk_stack_get_visible_child(GTK_STACK(stack));
  if (visible == file.page) {
    std::ifstream binary(std::string(gtk_editable_get_text(GTK_EDITABLE(file.entry))), std::ios::binary);
    if (!binary) {
      throw std::runtime_error("Failed to open file");
    }
    // Header
    std::vector<uint8_t> header(5+5+4+8);
    binary.read(reinterpret_cast<char*>(header.data()), 5+5+4+8);
    if (binary.gcount() < 5+5+4+8) {
      throw std::runtime_error("File is too short");
    }
    //// Extract
    std::string program(reinterpret_cast<const char*>(header.data()), 5);
    std::string trace(reinterpret_cast<const char*>(header.data()+5), 5);
    uint32_t version = *reinterpret_cast<uint32_t*>(header.data()+5+5);
    uint64_t timestamp = *reinterpret_cast<uint64_t*>(header.data()+5+5+4);
    //// Check
    if (program != "DOETH") {
      throw std::runtime_error("Unexpected file type! Got: "+program+" but DOETH expected.");
    }
    std::cout << "Reading trace of type "+trace << std::endl;
    // Payload
    (*buffer) = std::vector<uint8_t>(std::istreambuf_iterator<char>(binary), std::istreambuf_iterator<char>());
  }
  else if (visible == ethernet.page) {
    //// Get destination from entry
    const char* path = gtk_editable_get_text(GTK_EDITABLE(ethernet.entry));
    if (g_file_test(path, G_FILE_TEST_EXISTS)) {
      std::string filename = "";
      if (g_file_test(path, G_FILE_TEST_IS_DIR)) {
        filename = std::string(path)+"/"+getCurrentTimestamp()+".bin";
      }
      else if (g_file_test(path, G_FILE_TEST_IS_REGULAR)) {
        filename = std::string(path);
      }
      else {
        throw std::runtime_error(std::string(path)+" exists but is neither a file nor a directory.");
      }
      //// Dump data
      ////// Open the file in binary mode
      std::ofstream out_file(filename, std::ios::binary);
      if (!out_file) {
        throw std::runtime_error("Failed to open file for writing.");
      }
      ////// Write header
      std::vector<std::string> metadata = {"DOETH", "TPIU ", "v0.1", "00000000"};
      for (const auto& meta : metadata)
        out_file.write(reinterpret_cast<const char*>(meta.data()), meta.size());
      ////// Write the entire buffer as binary data
      out_file.write(reinterpret_cast<const char*>(buffer->data()), buffer->size());
      out_file.close();
    }
    else {
      throw std::runtime_error("Failed to open file for writing.");
    }
  }
}


class ParseTracePage: public Page {

  private:
    std::atomic<double> progress_value{0.0};
    GtkWidget* title_row;
    GtkWidget* title;
    GtkWidget* progress;
    GtkWidget* percent_label;
    GtkWidget* spinner;
    std::vector<uint8_t>* buffer;
    Deformatter* deformatter;

  public:
    ParseTracePage(std::vector<uint8_t>* buffer, Deformatter* deformatter);
    void onActivate(GtkWidget* button);

};

ParseTracePage::ParseTracePage(std::vector<uint8_t>* buffer, Deformatter* deformatter): Page("Parse Trace"), buffer(buffer), deformatter(deformatter) {
  page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 15);
  gtk_widget_set_valign(page, GTK_ALIGN_CENTER);
  gtk_widget_set_halign(page, GTK_ALIGN_CENTER);
  /* Title row (title + spinner) */
  title_row = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  gtk_widget_set_halign(title_row, GTK_ALIGN_CENTER);
  //// Title
  title = gtk_label_new("Parsing in progress");
  gtk_widget_add_css_class(title, "title-1");
  gtk_label_set_justify(GTK_LABEL(title), GTK_JUSTIFY_CENTER);
  //// Spinner
  spinner = gtk_spinner_new();
  //// Pack title row
  gtk_box_append(GTK_BOX(title_row), title);
  gtk_box_append(GTK_BOX(title_row), spinner);
  // Progress bar
  progress = gtk_progress_bar_new();
  gtk_widget_set_size_request(progress, 400, 25);
  gtk_widget_set_halign(progress, GTK_ALIGN_CENTER);
  // Percentage label
  percent_label = gtk_label_new("0 %");
  gtk_widget_set_halign(percent_label, GTK_ALIGN_CENTER);
  // Pack all
  gtk_box_append(GTK_BOX(page), title_row);
  gtk_box_append(GTK_BOX(page), progress);
  gtk_box_append(GTK_BOX(page), percent_label);
}

void ParseTracePage::onActivate(GtkWidget* button) {
  progress_value = 0.0;
  gtk_widget_set_sensitive(button, FALSE);
  gtk_spinner_start(GTK_SPINNER(spinner));
  std::thread([this, button]() {
    int step = buffer->size()/100;
    for (long unsigned int i = 0; i < buffer->size(); i += step) {
      long unsigned int nextStep = std::min(i+step, buffer->size());
      for (long unsigned int j = i; j < nextStep; j++) {
        deformatter->insert(buffer->at(j));
      }
      progress_value = nextStep/((double)buffer->size());
      g_idle_add([](gpointer data) -> gboolean {
          auto* tuple = static_cast<std::pair<ParseTracePage*, GtkWidget*>*>(data);
          auto* self = tuple->first;
          GtkWidget* button = tuple->second;
          double f = self->progress_value.load();
          gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(self->progress), f);
          char buffer[16];
          snprintf(buffer, sizeof(buffer), "%.0f %%", f * 100);
          gtk_label_set_text(GTK_LABEL(self->percent_label), buffer);
          if (f >= 1.0) {
            gtk_spinner_stop(GTK_SPINNER(self->spinner));
            gtk_widget_set_sensitive(button, TRUE);
          }
          delete tuple;
          return G_SOURCE_REMOVE;
        },
        new std::pair<ParseTracePage*, GtkWidget*>{this, button}
      );
    }
  }).detach();
}

class Wizard {
  
  private:
    // Attributes
    GtkWidget* parent;
    GtkWidget* dialog;
    GtkWidget* content;
    GtkWidget* paned; 
    GtkWidget* stack;
    GtkWidget* step_list;
    GtkWidget* header;
    GtkWidget* next_button;
    std::vector<Page*>::iterator current;
    std::vector<Page*> steps;
    // Methods
    static void cOnNext(GtkWidget* _, gpointer data);

  public:
    Wizard(GtkApplication* app, GtkWidget* parent, std::vector<uint8_t>* buffer, Sniffer* sniffer, Deformatter* deformatter);
    void onNext();

};

Wizard::Wizard(GtkApplication* app, GtkWidget* parent, std::vector<uint8_t>* buffer, Sniffer* sniffer, Deformatter* deformatter): parent(parent) {
  // create DialogBox
  dialog = gtk_dialog_new();
  gtk_window_set_application(GTK_WINDOW(dialog), app);
  gtk_window_set_title(GTK_WINDOW(dialog), "Trace Setup Wizard");
  gtk_window_set_default_size(GTK_WINDOW(dialog), 700, 400);
  gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  // Create header for dialog box
  header = gtk_header_bar_new();
  gtk_header_bar_set_show_title_buttons(GTK_HEADER_BAR(header), FALSE);
  // Create button
  next_button = gtk_button_new_with_label("Next");
  g_signal_connect(next_button, "clicked", G_CALLBACK(Wizard::cOnNext), this);
  gtk_header_bar_pack_end(GTK_HEADER_BAR(header), next_button);
  gtk_window_set_titlebar(GTK_WINDOW(dialog), header);
  // Construct pane/tab system
  content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  paned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(content), paned);
  // Left pane
  step_list = gtk_list_box_new();
  gtk_paned_set_position(GTK_PANED(paned), 150);
  gtk_paned_set_start_child(GTK_PANED(paned), step_list);
  gtk_paned_set_resize_start_child(GTK_PANED(paned), FALSE);
  gtk_paned_set_shrink_start_child(GTK_PANED(paned), FALSE);
  // Right pane
  stack = gtk_stack_new();
  gtk_widget_set_hexpand(stack, TRUE);
  gtk_widget_set_vexpand(stack, TRUE);
  gtk_paned_set_end_child(GTK_PANED(paned), stack);
  // Setup steps
  steps.push_back(new StartPage());
  steps.push_back(new AcquireTracePage(buffer, sniffer));
  steps.push_back(new ParseTracePage(buffer, deformatter));
  //// Connect step-page
  for (const auto& page : steps) {
    gtk_list_box_append(GTK_LIST_BOX(step_list), page->step.row);
    gtk_stack_add_named(GTK_STACK(stack), page->page, page->getName().c_str());
  }
  //// Setup first step-page
  current = steps.begin();
  (*current)->step.markAsVisited();
  gtk_stack_set_visible_child_name(GTK_STACK(stack), (*current)->step.getName().c_str());
  // Show dialog box
  gtk_window_present(GTK_WINDOW(dialog));
}

void Wizard::onNext() {
  if (!steps.empty() && (current == std::prev(std::prev(steps.end())))) {
    (*current)->onActivate(GTK_WIDGET(next_button));
  }
  // Go to next step-page
  current++;
  if (current != steps.end()) {
    (*current)->step.markAsVisited();
    gtk_stack_set_visible_child_name(GTK_STACK(stack), (*current)->step.getName().c_str());
  }
  // Simple linear example
  if (!steps.empty() && (current == std::prev(steps.end()))) {
    // Update button style
    gtk_button_set_label(GTK_BUTTON(next_button), "Plot");
    GtkStyleContext* ctx = gtk_widget_get_style_context(GTK_WIDGET(next_button));
    gtk_style_context_add_class(ctx, "suggested-action");
    // Start thread
    (*current)->onActivate(GTK_WIDGET(next_button));
  }
  else if (current == steps.end()) {
    gtk_window_destroy(GTK_WINDOW(dialog));
    panels->tracker.update();
  }
}

void Wizard::cOnNext(GtkWidget* _, gpointer data) {
  Wizard* self = static_cast<Wizard*>(data);
  self->onNext();
}


static void onActivate(GtkApplication* app, gpointer _) {
  std::vector<uint8_t>* buffer = new std::vector<uint8_t>();
  Sniffer* sniffer = new Sniffer(buffer);
  Deformatter* deformatter = new Deformatter();

  GtkWidget* window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "DoEth Live Tracer");
  gtk_window_set_default_size(GTK_WINDOW(window), 1600, 900);
  gtk_window_present(GTK_WINDOW(window));

  GtkWidget* notebook = gtk_notebook_new();

  Wizard* wiz = new Wizard(app, window, buffer, sniffer, deformatter);
  panels = new SourcePanels();
  roofline = new RooflinePanel();

  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), panels->parent, gtk_label_new("Global Monitor"));
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), roofline->parent, gtk_label_new("Roofline"));
  gtk_window_set_child(GTK_WINDOW(window), notebook);
}


int main(int argc, char** argv) {
  GtkApplication* app = gtk_application_new("com.example.wizard", G_APPLICATION_FLAGS_NONE);

  g_signal_connect(app, "activate", G_CALLBACK(onActivate), NULL);

  int status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  return status;
}
