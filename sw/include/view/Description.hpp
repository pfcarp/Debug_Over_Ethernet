#pragma once


#include <gtk/gtk.h>
#include <string>


class Description {
  
  private:
    // Attributes
    GtkWidget* label;
    std::string content = "";
    // Methods
    Description() = default;

  public:
    static Description& instance() {
        static Description instance;
        return instance;
    }

    void link(GtkWidget* target) {
      label = target;
    }

    void reset() {
      content = "";
    }

    void add(const std::string& msg) {
      content += msg;
    }

    void update() {
      gtk_label_set_text(GTK_LABEL(label), content.c_str());
    }

    Description(const Description&) = delete;
    Description& operator=(const Description&) = delete;

};
