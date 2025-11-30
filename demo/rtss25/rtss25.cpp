#include <cstdint>
#include <iostream>
#include <cstdlib>

#include <opencv2/opencv.hpp>
#include "tensorflow/lite/micro/micro_interpreter.h"
#include "tensorflow/lite/micro/system_setup.h"
#include "model_settings.h"
#include "gen/include/person_detect_model_data.h"
#include "gen/include/gen_micro_mutable_op_resolver.h"

constexpr int kTensorArenaSize = 1024*1024;
alignas(16) static uint8_t tensor_arena[kTensorArenaSize];

class Pipeline {

  private:
    // Tensorflow
    const tflite::Model* model = nullptr;
    tflite::MicroInterpreter* interpreter = nullptr;
    TfLiteTensor* input = nullptr;

    int in_h, in_w, in_c;
    uint64_t image_size;
    // Create VideoCapture with a backend (optional but safer)
    cv::VideoCapture cap;
    // Frames
    cv::Mat frame;
    cv::Mat filtered;
    // Filters
    void sepia();
    void sobel();
    void threshold();
    void duplicate();
  
  public:
    Pipeline(int index);
    bool capture();
    void filter(int mode);
    bool detect();
    void display();
    void release();

};

Pipeline::Pipeline(int index) {
  // Create a resizable window
  cv::namedWindow("Webcam Feed", cv::WINDOW_NORMAL);
  // Create VideoCapture with a backend (optional but safer)
  cap = cv::VideoCapture(index);
  if (!cap.isOpened()) {
    // TODO: switch to throw
    std::cerr << "Error: Could not open camera." << std::endl;
  }
  // Tensorflow
  tflite::InitializeTarget();

  // Map the model into a usable data structure. This doesn't involve any
  // copying or parsing, it's a very lightweight operation.
  model = tflite::GetModel(person_detect_tflite);
  if (model->version() != TFLITE_SCHEMA_VERSION) {
    MicroPrintf(
        "Model provided is schema version %d not equal "
        "to supported version %d.",
        model->version(), TFLITE_SCHEMA_VERSION);
    return;
  }

  // Pull in only the operation implementations we need.
  // This relies on a complete list of all the ops needed by this graph.

  // NOLINTNEXTLINE(runtime-global-variables)
  static tflite::MicroMutableOpResolver<5> micro_op_resolver;
  micro_op_resolver.AddAveragePool2D(tflite::Register_AVERAGE_POOL_2D_INT8());
  micro_op_resolver.AddConv2D(tflite::Register_CONV_2D_INT8());
  micro_op_resolver.AddDepthwiseConv2D(tflite::Register_DEPTHWISE_CONV_2D_INT8());
  micro_op_resolver.AddReshape();
  micro_op_resolver.AddSoftmax(tflite::Register_SOFTMAX_INT8());

  // Build an interpreter to run the model with.
  // NOLINTNEXTLINE(runtime-global-variables)
  static tflite::MicroInterpreter static_interpreter(
      model, micro_op_resolver, tensor_arena, kTensorArenaSize);
  interpreter = &static_interpreter;

  // Allocate memory from the tensor_arena for the model's tensors.
  TfLiteStatus allocate_status = interpreter->AllocateTensors();
  if (allocate_status != kTfLiteOk) {
    MicroPrintf("AllocateTensors() failed");
    return;
  }

  // Get information about the memory area to use for the model's input.
  input = interpreter->input(0);
}

bool Pipeline::capture() {
  cap >> frame;
  return frame.empty();
}

void Pipeline::sepia() {
  cv::Mat kernel = (cv::Mat_<float>(3,3) << 0.272, 0.534, 0.131, 0.349, 0.686, 0.168, 0.393, 0.769, 0.189);
  cv::transform(frame, filtered, kernel);
  cv::convertScaleAbs(filtered, filtered);
}

void Pipeline::sobel() {
  cv::Mat gray, gx, gy;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::Sobel(gray, gx, CV_16S, 1, 0);
  cv::Sobel(gray, gy, CV_16S, 0, 1);
  cv::convertScaleAbs(gx, gx);
  cv::convertScaleAbs(gy, gy);
  cv::addWeighted(gx, 0.5, gy, 0.5, 0, filtered);
  cv::cvtColor(filtered, filtered, cv::COLOR_GRAY2BGR);
}

void Pipeline::threshold() {
  cv::Mat gray;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::threshold(gray, filtered, 100, 255, cv::THRESH_BINARY);
  cv::cvtColor(filtered, filtered, cv::COLOR_GRAY2BGR);
}

void Pipeline::duplicate() {
  filtered = frame.clone();
}

void Pipeline::filter(int mode) {
  switch (mode) {
    case 1:
      sepia();
      break;
    case 2:
      sobel();
      break;
    case 3:
      threshold();
      break;
    default:
      duplicate();
  }
}

bool Pipeline::detect() {
  // Preprocess: resize + RGB
  cv::Mat gray, resized;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::resize(gray, resized, cv::Size(in_w, in_h));
  // Transfer
  memcpy(input->data.uint8, resized.data, in_w*in_h);
  // Inference
  std::cout << "Inference!" << std::endl;
  if (kTfLiteOk != interpreter->Invoke()) {
    std::cerr << "Inference failed." << std::endl;
    return false;
  }
  // Get output
  std::cout << "Get output" << std::endl;
  TfLiteTensor* output = interpreter->output(0);
  int8_t person_score = output->data.uint8[kPersonIndex];
  int8_t no_person_score = output->data.uint8[kNotAPersonIndex];
  MicroPrintf("Score: %u, %u\n", person_score, no_person_score);
  return person_score > 128;
}

void Pipeline::display() {
  cv::imshow("Webcam Feed", filtered);
}

void Pipeline::release() {
  cap.release();
}


int main(int argc, char** argv) {
    
  int camIndex = 0;
  if (argc > 1) {
    camIndex = std::atoi(argv[1]);
  }

  int mode = 0;
  std::cout << "Press '1' Sepia, '2' Sobel, '3' Threshold, '0' None, 'q' Quit\n";

  Pipeline pipeline(camIndex);

  while (true) {
    
    char key = (char)cv::waitKey(1);
    if (key == 'q') break;
    else if (key == '1') mode = 1;
    else if (key == '2') mode = 2;
    else if (key == '3') mode = 3;
    else if (key == '0') mode = 0;
    
    if (pipeline.capture()) {
      break;
    }
    //if (pipeline.detect()) {
      pipeline.filter(mode);
      //pipeline.compress();
      //pipeline.store();
    //}
    pipeline.display();
  }

  cv::destroyAllWindows();

  return 0;
}

