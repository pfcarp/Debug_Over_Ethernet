CXXFLAGS=-lm
GTK4FLAGS=`pkg-config --cflags --libs gtk4`
GRAPHVIZFLAGS=-lgvc -lcgraph
OPT=-O0
DEBUG=-g

MODEL=src/model
VIEW=src/view
CTRL=src/control

INCLUDE=-Iinclude/view -Iinclude/model -Iinclude/control
OBJ=object


all: gui parser


Color.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Color.cpp -o $(OBJ)/Color.o

Event.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Event.cpp -o $(OBJ)/Event.o

Point.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Point.cpp -o $(OBJ)/Point.o

Inserter.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(CTRL)/Inserter.cpp -o $(OBJ)/Inserter.o

Collection.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Collection.cpp -o $(OBJ)/Collection.o

Points.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Points.cpp -o $(OBJ)/Points.o

DataBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/DataBuffer.cpp -o $(OBJ)/DataBuffer.o $(CXXFLAGS)

HistogramBuffer.o: Color.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/HistogramBuffer.cpp -o $(OBJ)/HistogramBuffer.o $(CXXFLAGS)

Controls.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/Controls.cpp -o $(OBJ)/Controls.o $(GTK4FLAGS)

PlotArea.o: Collection.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/PlotArea.cpp -o $(OBJ)/PlotArea.o $(GTK4FLAGS)

GraphEdge.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/GraphEdge.cpp -o $(OBJ)/GraphEdge.o

GraphNode.o:
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/GraphNode.cpp -o $(OBJ)/GraphNode.o

Graph.o: GraphEdge.o GraphNode.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(MODEL)/Graph.cpp -o $(OBJ)/Graph.o

GraphArea.o: Graph.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) -c $(VIEW)/GraphArea.cpp -o $(OBJ)/GraphArea.o $(GTK4FLAGS)

gui: DataBuffer.o HistogramBuffer.o Controls.o Color.o PlotArea.o Collection.o Inserter.o Points.o Point.o GraphArea.o Graph.o GraphNode.o GraphEdge.o Event.o
	g++ $(DEBUG) $(OPT) $(INCLUDE) gui.c $(OBJ)/*.o -o doeth.out $(GTK4FLAGS) $(GRAPHVIZFLAGS)


packetfactory.o:
	g++ $(DEBUG) $(OPT) -c PacketFactory.cpp -o $(OBJ)/PacketFactory.o

parser: packetfactory.o
	g++ $(DEBUG) $(OPT) parser.cpp $(OBJ)/*.o -o parser.out


clean:
	rm -f doeth.out parser.out
	rm -f $(OBJ)/*.o

