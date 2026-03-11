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
    static Description& instance();
    Description(const Description&) = delete;
    Description& operator=(const Description&) = delete;
    void link(GtkWidget* target);
    void reset();
    void add(const std::string& msg);
    void update();

};
