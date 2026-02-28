#include <gtk/gtk.h>
#include <vector>
#include <string>
#include <map>
#include <iostream>
#include <thread>


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
    static void on_browse_clicked(GtkButton *btn, gpointer user_data);
    static void on_file_selected(GObject *source, GAsyncResult *res, gpointer data);

  public:
    AcquireTracePage();

};

void AcquireTracePage::on_browse_clicked(GtkButton *btn, gpointer user_data) {
  GtkWidget *entry = GTK_WIDGET(user_data);
  GtkFileDialog *dialog = gtk_file_dialog_new();
  gtk_file_dialog_open(dialog, NULL, NULL, on_file_selected, entry);
}

void AcquireTracePage::on_file_selected(GObject *source, GAsyncResult *res, gpointer data) {
  GtkWidget *entry = GTK_WIDGET(data);
  GFile *file = gtk_file_dialog_open_finish(GTK_FILE_DIALOG(source), res, NULL);
  if (file) {
    char *path = g_file_get_path(file);
    gtk_editable_set_text(GTK_EDITABLE(entry), path);
    g_free(path);
    g_object_unref(file);
  }
  g_object_unref(source);
}

AcquireTracePage::AcquireTracePage(): Page("Acquire Trace") {
  page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);

  // Stack container
  GtkWidget* stack = gtk_stack_new();
  gtk_widget_set_vexpand(stack, TRUE);
  gtk_widget_set_hexpand(stack, TRUE);

  // Switcher (tabs header)
  GtkWidget* switcher = gtk_stack_switcher_new();
  gtk_stack_switcher_set_stack(GTK_STACK_SWITCHER(switcher), GTK_STACK(stack));

  //// File Page
  GtkWidget* file_page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
  /* Horizontal row for entry + button */
  GtkWidget* file_row = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  GtkWidget* file_entry = gtk_entry_new();
  gtk_widget_set_hexpand(file_entry, TRUE);
  //// Interface label
  GtkWidget* file_title = gtk_label_new("File selection.");
  gtk_widget_add_css_class(file_title, "heading");
  // Description
  GtkWidget* file_desc = gtk_label_new("Select the source trace file of interrest by clicking the 'Browse' button. Make sure it is correct.");
  gtk_label_set_wrap(GTK_LABEL(file_desc), TRUE);
  gtk_label_set_justify(GTK_LABEL(file_desc), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(file_desc, GTK_ALIGN_CENTER);
  ////// Create browse button
  GtkWidget* browse_btn = gtk_button_new_with_label("Browse");
  ////// Store entry pointer inside button for callback access
  g_object_set_data(G_OBJECT(browse_btn), "entry", file_entry);
  ////// Browse callback
  g_signal_connect(browse_btn, "clicked", G_CALLBACK(on_browse_clicked), file_entry);
  //// Assemble row
  gtk_box_append(GTK_BOX(file_row), file_entry);
  gtk_box_append(GTK_BOX(file_row), browse_btn);
  //// Add to page
  gtk_box_append(GTK_BOX(file_page), file_title);
  gtk_box_append(GTK_BOX(file_page), file_desc);
  gtk_box_append(GTK_BOX(file_page), file_row);
  // Add to stack
  gtk_stack_add_titled(GTK_STACK(stack), file_page, "file", "Load from file");

  // Ethernet Page
  GtkWidget* eth_page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
  gtk_stack_add_titled(GTK_STACK(stack), eth_page, "ethernet", "Load over Ethernet");
  //// Interface label
  GtkWidget* iface_title = gtk_label_new("Ethernet interface selection.");
  gtk_widget_add_css_class(iface_title, "heading");
  gtk_box_append(GTK_BOX(eth_page), iface_title);
  // Description
  GtkWidget* iface_desc1 = gtk_label_new("Select the interface to listen to to get the trace.");
  gtk_label_set_wrap(GTK_LABEL(iface_desc1), TRUE);
  gtk_label_set_justify(GTK_LABEL(iface_desc1), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(iface_desc1, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(eth_page), iface_desc1);
  //// TOP ROW : Interface + Controls
  GtkWidget* top_row = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 8);
  ////// Combo box text
  GtkWidget* combo = gtk_combo_box_text_new();
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(combo), NULL, "eth0");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(combo), NULL, "eth1");
  gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(combo), NULL, "lo");
  gtk_widget_set_hexpand(combo, TRUE);
  gtk_box_append(GTK_BOX(top_row), combo);
  ////// Play button
  GtkWidget* play_btn = gtk_button_new_from_icon_name("media-playback-start");
  gtk_box_append(GTK_BOX(top_row), play_btn);
  ////// Stop button
  GtkWidget* stop_btn = gtk_button_new_from_icon_name("media-playback-stop");
  gtk_box_append(GTK_BOX(top_row), stop_btn);
  ////// Push top row into main box
  gtk_box_append(GTK_BOX(eth_page), top_row);
  //// TRACE SAVING SECTION
  ////// Title label
  GtkWidget* save_title = gtk_label_new("Save trace?");
  gtk_widget_add_css_class(save_title, "heading");
  gtk_box_append(GTK_BOX(eth_page), save_title);
  ////// Description
  GtkWidget* iface_desc2 = gtk_label_new("Select a destination to store the recoding of the trace by clicking the 'Browse' button. Confirm the action by checking the box below.");
  gtk_label_set_wrap(GTK_LABEL(iface_desc2), TRUE);
  gtk_label_set_justify(GTK_LABEL(iface_desc2), GTK_JUSTIFY_CENTER);
  gtk_widget_set_halign(iface_desc2, GTK_ALIGN_CENTER);
  gtk_box_append(GTK_BOX(eth_page), iface_desc2);
  ////// File picker row
  GtkWidget* file_row2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
  GtkWidget* file_entry2 = gtk_entry_new();
  gtk_widget_set_hexpand(file_entry2, TRUE);
  GtkWidget* browse_btn2 = gtk_button_new_with_label("Browse");
  ////// Connect browse button (reuse your previous pattern) */
  g_signal_connect(browse_btn2, "clicked", G_CALLBACK(on_browse_clicked), file_entry2);
  gtk_box_append(GTK_BOX(file_row2), file_entry2);
  gtk_box_append(GTK_BOX(file_row2), browse_btn2);
  gtk_box_append(GTK_BOX(eth_page), file_row2);
  ////// Checkbox
  GtkWidget* save_check = gtk_check_button_new_with_label("Enable saving");
  gtk_box_append(GTK_BOX(eth_page), save_check);
  // Add switcher + stack to page
  gtk_box_append(GTK_BOX(page), switcher);
  gtk_box_append(GTK_BOX(page), stack);
}


class ParseTracePage: public Page {

  private:
    std::atomic<double> progress_value{0.0};
    GtkWidget* title_row;
    GtkWidget* title;
    GtkWidget* progress;
    GtkWidget* percent_label;
    GtkWidget* spinner;

  public:
    ParseTracePage();
    void onActivate(GtkWidget* button);

};

ParseTracePage::ParseTracePage(): Page("Parse Trace") {
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
    for (int i = 0; i <= 100; ++i) {
      progress_value = i / 100.0;
      g_idle_add([](gpointer data) -> gboolean {
        auto* tuple = static_cast<std::pair<ParseTracePage*, GtkWidget*>*>(data);
        auto* self = tuple->first;
        GtkWidget* button = tuple->second;
        double f = self->progress_value.load();
        gtk_progress_bar_set_fraction(
          GTK_PROGRESS_BAR(self->progress), f);
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
      new std::pair<ParseTracePage*, GtkWidget*>{this, button});
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
  }).detach();
}

struct Wizard {
  
  GtkWidget* dialog;
  GtkWidget* stack;
  GtkWidget* step_list;
  GtkWidget* header;
  GtkWidget* next_button;

  std::vector<Page*>::iterator current;
  std::vector<Page*> steps;
};


static void on_next(GtkWidget* _, gpointer data) {
  Wizard* wiz = static_cast<Wizard*>(data);

  wiz->current++;
  if (wiz->current != wiz->steps.end()) {
    (*wiz->current)->step.markAsVisited();
    gtk_stack_set_visible_child_name(GTK_STACK(wiz->stack), (*wiz->current)->step.getName().c_str());
  }

  // Simple linear example
  if (!wiz->steps.empty() && (wiz->current == std::prev(wiz->steps.end()))) {
    // Update button style
    gtk_button_set_label(GTK_BUTTON(wiz->next_button), "Plot");
    GtkStyleContext* ctx = gtk_widget_get_style_context(GTK_WIDGET(wiz->next_button));
    gtk_style_context_add_class(ctx, "suggested-action");
    // Start thread
    (*wiz->current)->onActivate(GTK_WIDGET(wiz->next_button));
  }
  else if (wiz->current == wiz->steps.end()) {
    gtk_window_destroy(GTK_WINDOW(wiz->dialog));
  }
}


static void on_activate(GtkApplication* app, gpointer) {
  Wizard* wiz = new Wizard();

  GtkWidget* window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "DoEth Live Tracer");
  gtk_window_set_default_size(GTK_WINDOW(window), 1000, 700);

  wiz->dialog = gtk_dialog_new();
  gtk_window_set_application(GTK_WINDOW(wiz->dialog), app);
  gtk_window_set_title(GTK_WINDOW(wiz->dialog), "Trace Setup Wizard");
  gtk_window_set_default_size(GTK_WINDOW(wiz->dialog), 700, 400);
  gtk_window_set_transient_for(GTK_WINDOW(wiz->dialog), GTK_WINDOW(window));
  gtk_window_set_modal(GTK_WINDOW(wiz->dialog), TRUE);


  wiz->header = gtk_header_bar_new();
  gtk_header_bar_set_show_title_buttons(GTK_HEADER_BAR(wiz->header), FALSE);

  wiz->next_button = gtk_button_new_with_label("Next");
  g_signal_connect(wiz->next_button, "clicked", G_CALLBACK(on_next), wiz);
  gtk_header_bar_pack_end(GTK_HEADER_BAR(wiz->header), wiz->next_button);
  gtk_window_set_titlebar(GTK_WINDOW(wiz->dialog), wiz->header);


  GtkWidget* content = gtk_dialog_get_content_area(GTK_DIALOG(wiz->dialog));

  GtkWidget* paned = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
  gtk_box_append(GTK_BOX(content), paned);

  // LEFT SIDE: Step list
  wiz->step_list = gtk_list_box_new();
  gtk_paned_set_position(GTK_PANED(paned), 150);
  gtk_paned_set_start_child(GTK_PANED(paned), wiz->step_list);
  gtk_paned_set_resize_start_child(GTK_PANED(paned), FALSE);
  gtk_paned_set_shrink_start_child(GTK_PANED(paned), FALSE);

  // RIGHT SIDE: Pages
  wiz->stack = gtk_stack_new();
  gtk_widget_set_hexpand(wiz->stack, TRUE);
  gtk_widget_set_vexpand(wiz->stack, TRUE);
  gtk_paned_set_end_child(GTK_PANED(paned), wiz->stack);

  // Create steps
  wiz->steps.push_back(new StartPage());
  wiz->steps.push_back(new AcquireTracePage());
  wiz->steps.push_back(new ParseTracePage());
  
  for (const auto& page : wiz->steps) {
    gtk_list_box_append(GTK_LIST_BOX(wiz->step_list), page->step.row);
    gtk_stack_add_named(GTK_STACK(wiz->stack), page->page, page->getName().c_str());
  }

  wiz->current = wiz->steps.begin();
  (*wiz->current)->step.markAsVisited();
  gtk_stack_set_visible_child_name(GTK_STACK(wiz->stack), (*wiz->current)->step.getName().c_str());

  gtk_window_present(GTK_WINDOW(window));
  gtk_window_present(GTK_WINDOW(wiz->dialog));
}


int main(int argc, char** argv) {
  GtkApplication* app = gtk_application_new("com.example.wizard", G_APPLICATION_FLAGS_NONE);

  g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);

  int status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  return status;
}
