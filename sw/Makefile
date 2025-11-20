CXXFLAGS=-lm -std=c++20 -Wall -Wextra
GTK4FLAGS=`pkg-config --cflags --libs gtk4`
GRAPHVIZFLAGS=-lgvc -lcgraph
OPT=-O0
DEBUG=-g

MODEL=src/model
VIEW=src/view
CTRL=src/control

TEST=test/

INCLUDE=-Iinclude/view -Iinclude/model -Iinclude/control
OBJ=object


all: gui parser tests


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
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Points.cpp -o $(OBJ)/Points. $(CXXFLAGS)

DataBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/DataBuffer.cpp -o $(OBJ)/DataBuffer.o $(CXXFLAGS)

HistogramBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/HistogramBuffer.cpp -o $(OBJ)/HistogramBuffer.o $(CXXFLAGS)

Controls.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/Controls.cpp -o $(OBJ)/Controls.o $(GTK4FLAGS) $(CXXFLAGS)

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

gui: DataBuffer.o HistogramBuffer.o Controls.o Color.o PlotArea.o Collection.o Inserter.o Points.o Point.o GraphArea.o Graph.o GraphNode.o GraphEdge.o Event.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) gui.c $(OBJ)/*.o -o doeth.out $(GTK4FLAGS) $(GRAPHVIZFLAGS) $(CXXFLAGS)


packet.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Packet.cpp -o $(OBJ)/Packet.o $(CXXFLAGS)

packetfactory.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/PacketFactory.cpp -o $(OBJ)/PacketFactory.o $(CXXFLAGS)

parser: packetfactory.o packet.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) parser.cpp $(OBJ)/*.o -o parser.out $(CXXFLAGS)


tests: packet.o
	g++ $(OPT) $(INCLUDE) tests.cpp $(TEST)/packet/*.cpp $(OBJ)/*.o -o tests.out $(CXXFLAGS)


clean:
	rm -f doeth.out parser.out tests.out
	rm -f $(OBJ)/*.o

