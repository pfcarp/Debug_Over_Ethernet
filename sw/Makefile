CXXFLAGS=-lm -std=c++20 -Wall -Wextra
GTK4FLAGS=`pkg-config --cflags --libs gtk4`
PCAPFLAGS=`pkg-config --cflags --libs libpcap`
GRAPHVIZFLAGS=-lgvc -lcgraph
OPT=-O2 -march=native
DEBUG=-g

MODEL=src/model
VIEW=src/view
CTRL=src/control

TEST=test/

INCLUDE=-Iinclude/view -Iinclude/model -Iinclude/control -Iinclude/common
OBJ=object


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

DataBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/DataBuffer.cpp -o $(OBJ)/DataBuffer.o $(CXXFLAGS)

HistogramBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/HistogramBuffer.cpp -o $(OBJ)/HistogramBuffer.o $(CXXFLAGS)

Controls.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/Controls.cpp -o $(OBJ)/Controls.o $(GTK4FLAGS) $(CXXFLAGS)

InterfaceSelector.o: Sniffer.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/InterfaceSelector.cpp -o $(OBJ)/InterfaceSelector.o $(GTK4FLAGS) $(CXXFLAGS)

PlotArea.o: Collection.o
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

PacketFactory.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/PacketFactory.cpp -o $(OBJ)/PacketFactory.o $(CXXFLAGS)

Stream.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Stream.cpp -o $(OBJ)/Stream.o $(CXXFLAGS)

Deformatter.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Deformatter.cpp -o $(OBJ)/Deformatter.o $(CXXFLAGS)

Dispatcher.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Dispatcher.cpp -o $(OBJ)/Dispatcher.o $(CXXFLAGS)


parser: PacketFactory.o Packet.o Stream.o Deformatter.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) parser.cpp $(OBJ)/*.o -o parser.out $(CXXFLAGS)

online: PacketFactory.o Packet.o Stream.o Deformatter.o Sniffer.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) online.cpp $(OBJ)/*.o -o online.out $(CXXFLAGS) $(PCAPFLAGS)

tests: Dispatcher.o Packet.o Deformatter.o Stream.o PacketFactory.o
	g++ $(OPT) $(INCLUDE) tests.cpp $(wildcard test/packet/*.cpp) $(wildcard test/deformatter/*.cpp) $(OBJ)/*.o -o tests.out $(CXXFLAGS)

gui: WatchPoint.o WatchPoints.o Events.o Dispatcher.o DataBuffer.o HistogramBuffer.o Controls.o Sniffer.o InterfaceSelector.o Color.o PlotArea.o Stream.o Deformatter.o Collection.o Inserter.o Points.o Point.o GraphArea.o Graph.o GraphNode.o GraphEdge.o Event.o Packet.o PacketFactory.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) gui.c $(wildcard object/*.o) -o doeth.out $(GTK4FLAGS) $(PCAPFLAGS) $(GRAPHVIZFLAGS) $(CXXFLAGS)

bench: PacketFactory.o Packet.o Stream.o Deformatter.o Dispatcher.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) bench.cpp $(OBJ)/*.o -o bench.out $(CXXFLAGS)

factory: PacketFactory.o Packet.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) factory.cpp $(OBJ)/*.o -o factory.out $(CXXFLAGS)


clean:
	rm -f doeth.out parser.out tests.out online.out
	rm -f $(OBJ)/*.o

