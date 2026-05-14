CXXFLAGS=-lm -std=c++20 -Wall -Wextra
GTK4FLAGS=`pkg-config --cflags --libs gtk4`
PCAPFLAGS=`pkg-config --cflags --libs libpcap`
GRAPHVIZFLAGS=-lgvc -lcgraph
OPT=-O2 -march=native
DEBUG=-g


MODEL=src/model
VIEW=src/view
CTRL=src/control


INCLUDE=-Iinclude/view -Iinclude/model -Iinclude/control -Iinclude/common
OBJ=object
BIN=bin


all: gui parser tests bench


Color.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Color.cpp -o $(OBJ)/Color.o $(CXXFLAGS)

Event.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Event.cpp -o $(OBJ)/Event.o $(CXXFLAGS)

Point.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Point.cpp -o $(OBJ)/Point.o $(CXXFLAGS)

Inserter.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Inserter.cpp -o $(OBJ)/Inserter.o $(CXXFLAGS)

Collection.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Collection.cpp -o $(OBJ)/Collection.o $(CXXFLAGS)

Points.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Points.cpp -o $(OBJ)/Points.o $(CXXFLAGS)

DataBuffer.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/DataBuffer.cpp -o $(OBJ)/DataBuffer.o $(CXXFLAGS)

HistogramBuffer.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/HistogramBuffer.cpp -o $(OBJ)/HistogramBuffer.o $(CXXFLAGS)

Controls.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/Controls.cpp -o $(OBJ)/Controls.o $(GTK4FLAGS) $(CXXFLAGS)

InterfaceSelector.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/InterfaceSelector.cpp -o $(OBJ)/InterfaceSelector.o $(GTK4FLAGS) $(CXXFLAGS)

PlotAreaTracker.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/PlotAreaTracker.cpp -o $(OBJ)/PlotAreaTracker.o $(GTK4FLAGS) $(CXXFLAGS)

PlotArea.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/PlotArea.cpp -o $(OBJ)/PlotArea.o $(GTK4FLAGS) $(CXXFLAGS)

GraphEdge.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/GraphEdge.cpp -o $(OBJ)/GraphEdge.o $(CXXFLAGS)

GraphNode.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/GraphNode.cpp -o $(OBJ)/GraphNode.o $(CXXFLAGS)

Graph.o: GraphEdge.o GraphNode.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Graph.cpp -o $(OBJ)/Graph.o $(CXXFLAGS)

GraphArea.o: Graph.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/GraphArea.cpp -o $(OBJ)/GraphArea.o $(GTK4FLAGS) $(CXXFLAGS)

Sniffer.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Sniffer.cpp -o $(OBJ)/Sniffer.o $(PCAPFLAGS) $(CXXFLAGS)

WatchPoint.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/WatchPoint.cpp -o $(OBJ)/WatchPoint.o $(CXXFLAGS)

WatchPoints.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/WatchPoints.cpp -o $(OBJ)/WatchPoints.o $(CXXFLAGS)

Events.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Events.cpp -o $(OBJ)/Events.o $(CXXFLAGS)

Packet.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Packet.cpp -o $(OBJ)/Packet.o $(CXXFLAGS)

Trace.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Trace.cpp -o $(OBJ)/Trace.o $(CXXFLAGS)

TraceCollection.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/TraceCollection.cpp -o $(OBJ)/TraceColletion.o $(CXXFLAGS)

TraceDatabase.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/TraceDatabase.cpp -o $(OBJ)/TraceDatabase.o $(CXXFLAGS)

PacketFactory.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/PacketFactory.cpp -o $(OBJ)/PacketFactory.o $(CXXFLAGS)

Stream.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Stream.cpp -o $(OBJ)/Stream.o $(CXXFLAGS)

Deformatter.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Deformatter.cpp -o $(OBJ)/Deformatter.o $(CXXFLAGS)

Dispatcher.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Dispatcher.cpp -o $(OBJ)/Dispatcher.o $(CXXFLAGS)

SourcePanel.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/SourcePanel.cpp -o $(OBJ)/SourcePanel.o $(GTK4FLAGS) $(CXXFLAGS)

SourcePanels.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/SourcePanels.cpp -o $(OBJ)/SourcePanels.o $(GTK4FLAGS) $(CXXFLAGS)

RooflinePanel.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/RooflinePanel.cpp -o $(OBJ)/RooflinePanel.o $(GTK4FLAGS) $(CXXFLAGS)

Description.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/Description.cpp -o $(OBJ)/Description.o $(GTK4FLAGS) $(CXXFLAGS)

TimemarkerDialog.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/TimemarkerDialog.cpp -o $(OBJ)/TimemarkerDialog.o $(GTK4FLAGS) $(CXXFLAGS)

Timemarker.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Timemarker.cpp -o $(OBJ)/Timemarker.o $(CXXFLAGS)

TimemarkerCollection.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/TimemarkerCollection.cpp -o $(OBJ)/TimemarkerCollection.o $(CXXFLAGS)


parser/offline: PacketFactory.o Packet.o Stream.o Deformatter.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) parser/offline.cpp $(OBJ)/*.o -o $(BIN)/offline-parser.out $(CXXFLAGS)

parser/online: PacketFactory.o Packet.o Stream.o Deformatter.o Sniffer.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) parser/online.cpp $(OBJ)/*.o -o $(BIN)/online-parser.out $(CXXFLAGS) $(PCAPFLAGS)

tests: Dispatcher.o Packet.o Deformatter.o Stream.o PacketFactory.o
	g++ $(OPT) $(INCLUDE) test/tests.cpp $(wildcard test/packet/*.cpp) $(wildcard test/deformatter/*.cpp) $(OBJ)/*.o -o $(BIN)/tests.out $(CXXFLAGS)

gui/online: WatchPoint.o WatchPoints.o Events.o Dispatcher.o DataBuffer.o HistogramBuffer.o Controls.o Sniffer.o InterfaceSelector.o Color.o PlotArea.o Stream.o Deformatter.o Collection.o Inserter.o Points.o Point.o GraphArea.o Graph.o GraphNode.o GraphEdge.o Event.o Packet.o PacketFactory.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) gui/online.cpp $(wildcard object/*.o) -o $(BIN)/online-gui.out $(GTK4FLAGS) $(PCAPFLAGS) $(GRAPHVIZFLAGS) $(CXXFLAGS)

gui/live: Deformatter.o PacketFactory.o Packet.o PlotAreaTracker.o PlotArea.o Color.o Trace.o TraceCollection.o TraceDatabase.o SourcePanel.o SourcePanels.o Description.o TimemarkerDialog.o TimemarkerCollection.o Timemarker.o RooflinePanel.o Sniffer.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) gui/live.cpp $(wildcard object/*.o) -o $(BIN)/live-gui.out $(GTK4FLAGS) $(CXXFLAGS) $(PCAPFLAGS)

benchmark/packet: PacketFactory.o Packet.o Stream.o Deformatter.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) benchmark/packet.cpp $(OBJ)/*.o -o $(BIN)/packet-benchmark.out $(CXXFLAGS)

benchmark/factory: PacketFactory.o Packet.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) benchmark/factory.cpp $(OBJ)/*.o -o $(BIN)/factory-benchmark.out $(CXXFLAGS)

listener/offline: Sniffer.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) listener/offline.cpp $(OBJ)/*.o -o $(BIN)/offline-listener.out $(CXXFLAGS) $(PCAPFLAGS)


clean:
	rm -f $(BIN)/*.out
	rm -f $(OBJ)/*.o

