import java.io.*;
import java.util.*;
/* This is a crucial part of the agent-based simulator used in King et. al 2022 
https://doi.org/10.1080/23249935.2021.2017510.  More details about how the 
simulator was used can be found there.  

This part of the simulator defines the crowd of agents.  It provides the 
functions and properties of the crowd, allowing the movement and choices made 
by agents to be updated.  As part of the latter, the occupancies of, distances 
from, and desirability of destinations are calculated as necessary.  

The force-based movement model based on Helbing et. al 2000 
https://www.nature.com/articles/35035023 is also implemented here.  

Several functions implementing different destination choice models are provided, 
but not used in the current version of the code (24/05/2022).
*/
public class IREV1 {

	// Declare variable to be used in the whole class by all methods/objects.
	// Initialise an array of agents.
	public agent[] people;
	/* 
	Initialise: 
		timestep size between updating positions, 
		the magnitude of random fluctuation in movement direction, 
		size of agent, and 
		set simulated time to 0. 
	*/
	public double deltaT, noise, size, time = 0.0;
	// Force-based model parameters.
	public double tau, kpara, kappa;
	// Destination choice model parameters.
	public double [] param;
	/* Initialise:
		total number of agents, 
		the number of cells in each row/column of simulated space, and
		the radius of the agents
	 */
	public int N, gridSize, R;
	// Initialise position of exit.
	public int[] exitx, exity;
	// Matrix for distances of grid cells from a given destination.
	public double[][] dists;
	// Storage for floor field and distance values for each destination.
	public double[][][] fstatic;
	public double[][][] distances;
	// Matrix for each grid cell specifying spatial availability for placing agents.
	public double[][] grid;
	// Spatial grid for floor field values.
	public double[][] floorField;
	// Local change to the floor field due to agents when placing new agents.
	public double[][] excluFloorField;
	// Coordinates of grid cell which an agent's centre of mass occupies.
	public int[] gridPos;
	// Gradient of floor field for calculating preferred direction for an agent.
	public double[] gradient;
	// Extent in x and y to which an agent overlaps with something.
	public double[] entOverlap;
	// Number of agents that overlap with something.
	public double numEntOverlap;
	// Used to calculate the total force acting directly on the body of an agent.
	//public double[] bodyForces;
	// Set whether to vary the mass, preferred speed and radii of the agents.
	public boolean varyMass = false, varySpeed = false, varySize = false;
	// Boolean for the new next destination constraint.
	private boolean newNextDest;
	/* Set a random number generator used to set variable mass/speed/size, 
	noise in agent direction and gradient perception, and assigning transition 
	matrix elements.*/
	public Random twistor;
	// Number of destination in scenario.
	public int numDest;
	// Transition matrix for Markov model.  Used only if necessary.
	public double [][] transMat;
	// Number of agents at each destination.
	public int[] destPopularity;
	// PrintWriter object for destination sequence output file.
	public PrintWriter pwout;
	// Contains all the activities that each destination can have.
	public char[][] destActs;
	// Busyness parameter.
	public double occParam;
	// Distance parameter.
	public double distParam;
	// Desirability parameter.
	public double desParam;
	// Parameter for waiting time distribution.
	public double expParam = -1;
	// Set the radius of destinations for counting agents.
	public int destSize = 20;

	/* Constructor method where no model parameters are provided.  Sets default 
	   values for the destination choice model parameters as zero.
		 Takes the following inputs:
		 1.	Number of agents.
		 2.	Size of the spatial grid.
		 3.	X-coordinates of all destinations.
		 4. Y-coordinates of all destinations.
		 5. List of all possible activities across destinations.
		 6.	List of available activities for each destination.
		 7. Is the new next destination constraint being applied?
	*/
	public IREV1(int N, int gridSize, int[] exitx, int[] exity, 
	ArrayList<Character> acts, char[][] destActs, boolean newDest) { 
		//System.out.println("IREV1 no model parameter constructor activated.");
		this.N = N; // number of agents
		this.gridSize = gridSize; // number of position cells
		this.exitx = exitx; // x coordinate of exit
		this.exity = exity; // y coordinate of exit
		this.destActs = destActs; // list of activities at each destination
		numDest = exitx.length; // number of destinations
		this.occParam = 0.0; // Busyness parameter
		this.distParam = 0.0; // Distance parameter
		this.desParam = 0.0; // Desirability parameter
		this.newNextDest = newDest; // new destination constraint

		// Define parameters you are unlikely to change in simulations:
		deltaT = 0.05; // size of the timesteps.
		R = 4; // radius of agent.  Note that R is actually R/10 in metres.
		noise = 0.2; // random noise for direction of movement
		tau = 0.5; // velocity adaptation time (see helbing2000, moussaid2011).
		kpara = 12000;
		kappa = 24000; // kpara and kappa from Helbing et al. 2000.

		// This is the width of one grid cell in metres.  
		// Numerator sets how many centimetres wide a cell is.
		size = (double) gridSize / 10;
		/* Set a random number generator used to set variable mass/speed/size, 
		noise in agent direction and gradient perception, and assigning transition 
		matrix elements.*/
		twistor = new Random();
		// Initiate the crowd of agents.
		people = new agent[N];
		// Initiate the body force acting on all agents.
		//bodyForces = new double[N];
		
		// For every agent in the simulation...
		for (int vv = 0; vv < N; vv++) {
			//...create the agent...
			people[vv] = new agent(numDest, acts, acts.size());
			//...and give it a first destination...
			people[vv].destination = ranDest(numDest);
			//...and an activity to perform at said destination.  
			// Defaults to first activity available.
			people[vv].currAct = destActs[people[vv].destination][0];
			// Add the destination to the agent's destination sequence.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].dests.add(people[vv].destination);
			// Add the first decision time.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].decisionTimes.add(0);
			// Initialise the bodily forces acting on this agent.
			//bodyForces[vv] = 0.0;
		}
		// Matrix for distances of grid cells from a given destination.
		dists = new double[N][N];
		// Floor field and distance for every grid position for every destination.
		fstatic = new double[numDest][gridSize][gridSize];
		distances = new double[numDest][gridSize][gridSize];

		// Matrix for each grid cell specifying spatial availability for placing agents.
		grid = new double[gridSize][gridSize];
		// Spatial grid for floor field values.
		floorField = new double[gridSize][gridSize];
		// Local change to the floor field due to agents when placing new agents.
		excluFloorField = new double[gridSize][gridSize];

		// Initialise the grid positions of an agent.
		gridPos = new int[2];
		gridPos[0] = 1;
		gridPos[1] = 1;

		// Initialise the array for storing the gradient of the floor field.
		gradient = new double[2];
		gradient[0] = 1.0;
		gradient[1] = 1.0;

		// Initialise the amount an agent overlaps in the x and y directions.
		entOverlap = new double[2];
		entOverlap[0] = 0.0;
		entOverlap[1] = 0.0;

		// Assign values to floor fields: a room without doors.
		// For each destination...
		for (int k = 0; k < numDest; k++) {
			//...for each row of the grid...
			for (int i = 0; i < gridSize; i++) {
				//...for each value in the row...
				for (int j = i; j < gridSize; j++) {
					//...for the first/last 4 cells in the row/column of space...
					if (i < 3 || i > (gridSize - 4) || j < 3 || j > (gridSize - 4)) {
						//...set floor field = 0.
						fstatic[k][i][j] = 0.0;
						fstatic[k][j][i] = 0.0;
					} else { // Otherwise...
						//...set floor field = 1.
						fstatic[k][i][j] = 1.0;
						fstatic[k][j][i] = 1.0;
					}
					// Initialise distances and spatial availability for every grid cell.
					distances[k][i][j] = 0;
					grid[i][j] = 0.0;
					grid[j][i] = 0.0;
					// Set the first floor field as above.
					floorField[i][j] = fstatic[0][i][j];
					floorField[j][i] = fstatic[0][j][i];
				}
			}
		}
	// Transition matrix initialised with dimensions = number of destinations.
	// Must initialise it despite it never being used.
	transMat = new double[numDest][numDest];
	}

	/* Constructor method for model parameters with Markov model.
		 Takes the following inputs:
		 1.	Number of agents.
		 2.	Size of the spatial grid.
		 3.	X-coordinates of all destinations.
		 4. Y-coordinates of all destinations.
		 5. Values of all destination choice model parameters.
		 6.	Transition matrix for the Markov model.
		 7. List of all possible activities across destinations.
		 8.	List of available activities for each destination.
		 9. Is the new next destination constraint being applied?
	*/
	public IREV1(int N, int gridSize, int[] exitx, int[] exity, double[] params, 
	double[][] transMat, ArrayList<Character> acts, char[][] destActs, 
	boolean newDest) { 
		//System.out.println("IREV1 Markov model constructor activated.");
		this.N = N; // number of agents
		this.gridSize = gridSize; // number of position cells
		this.exitx = exitx; // x coordinate of exit
		this.exity = exity; // y coordinate of exit
		this.transMat = transMat; // Transition matrix
		this.destActs = destActs; // Activities available at each destination
		this.occParam = params[0]; // Busyness parameter
		this.distParam = params[1]; // Distance parameter
		this.desParam = params[2]; // Desirability parameter
		numDest = exitx.length; // Number of destinations
		this.newNextDest = newDest; // new destination constraint
		
		// Define parameters you are unlikely to change in simulations:
		deltaT = 0.05; // size of the timesteps.
		R = 4; // radius of agent.  Note that R is actually R/10 in units...
		noise = 0.2; // random noise for direction of movement
		tau = 0.5; // velocity adaptation time (see helbing2000, moussaid2011).
		kpara = 12000;
		kappa = 24000; // kpara and kappa from helbing2000.

		// This is the width of one grid cell in metres.  
		// Numerator sets how many centimetres wide a cell is.
		size = (double) gridSize / 10;
		/* Set a random number generator used to set variable mass/speed/size, 
		noise in agent direction and gradient perception, and assigning transition 
		matrix elements.*/
		twistor = new Random();
		// Initiate the crowd of agents.
		people = new agent[N];
		// Initiate the body force acting on all agents.
		//bodyForces = new double[N];

		// For every agent in the simulation...
		for (int vv = 0; vv < N; vv++) {
			//...create the agent...
			people[vv] = new agent(numDest, acts, acts.size());
			//...and give it a first destination...
			people[vv].destination = ranDest(numDest);
			//...and an activity to perform at said destination.  
			// Defaults to first activity available.
			people[vv].currAct = destActs[people[vv].destination][0];
			// Add the destination to the agent's destination sequence.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].dests.add(people[vv].destination);
			// Add the first decision time.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].decisionTimes.add(0);
			// Initialise the bodily forces acting on this agent.
			//bodyForces[vv] = 0.0;
		}
		// Matrix for distances of grid cells from a given destination.
		dists = new double[N][N];
		// Floor field and distance for every grid position for every destination.
		fstatic = new double[numDest][gridSize][gridSize];
		distances = new double[numDest][gridSize][gridSize];

		// Matrix for each grid cell specifying spatial availability for placing agents.
		grid = new double[gridSize][gridSize];
		// Spatial grid for floor field values.
		floorField = new double[gridSize][gridSize];
		// Local change to the floor field due to agents when placing new agents.
		excluFloorField = new double[gridSize][gridSize];

		// Initialise the grid positions of an agent.
		gridPos = new int[2];
		gridPos[0] = 1;
		gridPos[1] = 1;

		// Initialise the array for storing the gradient of the floor field.
		gradient = new double[2];
		gradient[0] = 1.0;
		gradient[1] = 1.0;

		// Initialise the amount an agent overlaps in the x and y directions.
		entOverlap = new double[2];
		entOverlap[0] = 0.0;
		entOverlap[1] = 0.0;

		// Assign values to floor fields: a room without doors.
		// For each destination...
		for (int k = 0; k < numDest; k++) {
			//...for each row of the grid...
			for (int i = 0; i < gridSize; i++) {
				//...for each value in the row...
				for (int j = i; j < gridSize; j++) {
					//...for the first/last 4 cells in the row/column of space...
					if (i < 3 || i > (gridSize - 4) || j < 3 || j > (gridSize - 4)) {
						//...set floor field = 0.
						fstatic[k][i][j] = 0.0;
						fstatic[k][j][i] = 0.0;
					} else { // Otherwise...
						//...set floor field = 1.
						fstatic[k][i][j] = 1.0;
						fstatic[k][j][i] = 1.0;
					}
					// Initialise distances and spatial availability for every grid cell.
					distances[k][i][j] = 0;
					grid[i][j] = 0.0;
					grid[j][i] = 0.0;
					// Set the first floor field as above.
					floorField[i][j] = fstatic[0][i][j];
					floorField[j][i] = fstatic[0][j][i];
				}
			}
		}
	}

	/* Constructor method with model parameters but no Markov.
		 Takes the following inputs:
			1.	Number of agents.
			2.	Size of the spatial grid.
			3.	X-coordinates of all destinations.
			4.	Y-coordinates of all destinations.
			5.	Values of all destination choice model parameters.
			6.	List of all possible activities across destinations.
			7.	List of available activities for each destination.
			8.	Is the new next destination constraint being applied?
	*/
	public IREV1(int N, int gridSize, int[] exitx, int[] exity, double [] params, 
	ArrayList<Character> acts, char[][] destActs, boolean newDest) { 
		//System.out.println("IREV1 full model constructor activated.");
		this.N = N; // number of agents
		this.gridSize = gridSize; // number of position cells
		this.exitx = exitx; // x coordinate of exit
		this.exity = exity; // y coordinate of exit
		this.destActs = destActs; // list of activities at each destination
		numDest = exitx.length; // number of destinations
		this.occParam = params[0]; // Busyness parameter
		this.distParam = params[1]; // Distance parameter
		this.desParam = params[2]; // Desirability parameter
		this.newNextDest = newDest; // new destination constraint

		// Define parameters you are unlikely to change in simulations:
		deltaT = 0.05; // size of the timesteps.
		R = 4; // radius of agent.  Note that R is actually R/10 in metres.
		noise = 0.2; // random noise for direction of movement
		tau = 0.5; // velocity adaptation time (see helbing2000, moussaid2011).
		kpara = 12000;
		kappa = 24000; // kpara and kappa from helbing2000.
		// This is the width of one grid cell in metres.  
		// Numerator sets how many centimetres wide a cell is.
		size = (double) gridSize / 10;
		/* Set a random number generator used to set variable mass/speed/size, 
		noise in agent direction and gradient perception, and assigning transition 
		matrix elements.*/
		twistor = new Random();
		// initiate the crowd of agents.
		people = new agent[N];
		// Initiate the body force acting on all agents.
		//bodyForces = new double[N];

		// For every agent in the simulation...
		for (int vv = 0; vv < N; vv++) {
			//...create the agent...
			people[vv] = new agent(numDest, acts, acts.size());
			//...and give it a first destination...
			people[vv].destination = ranDest(numDest);
			//...and an activity to perform at said destination.  
			// Defaults to first activity available.
			people[vv].currAct = destActs[people[vv].destination][0];
			// Add the destination to the agent's destination sequence.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].dests.add(people[vv].destination);
			// Add the first decision time.
			// Only used if this initial choice is to be counted as a destination chosen.
			//people[vv].decisionTimes.add(0);
			// Initialise the bodily forces acting on this agent.
			//bodyForces[vv] = 0.0;
		}
		// Matrix for distances of grid cells from a given destination.
		dists = new double[N][N];
		// Floor field and distance for every grid position for every destination.
		fstatic = new double[numDest][gridSize][gridSize];
		distances = new double[numDest][gridSize][gridSize];

		// Matrix for each grid cell specifying spatial availability for placing agents.
		grid = new double[gridSize][gridSize];
		// Spatial grid for floor field values.
		floorField = new double[gridSize][gridSize];
		// Local change to the floor field due to agents when placing new agents.
		excluFloorField = new double[gridSize][gridSize];

		// Initialise the grid positions of an agent.
		gridPos = new int[2];
		gridPos[0] = 1;
		gridPos[1] = 1;

		// Initialise the array for storing the gradient of the floor field.
		gradient = new double[2];
		gradient[0] = 1.0;
		gradient[1] = 1.0;

		// Initialise the amount an agent overlaps in the x and y directions.
		entOverlap = new double[2];
		entOverlap[0] = 0.0;
		entOverlap[1] = 0.0;

		// Assign values to floor fields: a room without doors.
		// For each destination...
		for (int k = 0; k < numDest; k++) {
			//...for each row of the grid...
			for (int i = 0; i < gridSize; i++) {
				//...for each value in the row...
				for (int j = i; j < gridSize; j++) {
					//...for the first/last 4 cells in the row/column of space...
					if (i < 3 || i > (gridSize - 4) || j < 3 || j > (gridSize - 4)) {
						//...set floor field = 0.
						fstatic[k][i][j] = 0.0;
						fstatic[k][j][i] = 0.0;
					} else { // Otherwise...
						//...set floor field = 1.
						fstatic[k][i][j] = 1.0;
						fstatic[k][j][i] = 1.0;
					}
					// Initialise distances and spatial availability for every grid cell.
					distances[k][i][j] = 0;
					grid[i][j] = 0.0;
					grid[j][i] = 0.0;
					// Set the first floor field as above.
					floorField[i][j] = fstatic[0][i][j];
					floorField[j][i] = fstatic[0][j][i];
				}
			}
		}
	}

	// This initialises agents if positions are given.  Not used.
	public void setInitialPosgiven() { 
		//System.out.println("Setting initial agent positions from file.");
		// For each agent...
		for (int i = 0; i < N; i++) {
			//...assign agent characteristics here:
			if (varyMass) { // agent mass
				people[i].mass = (double) twistor.nextInt(41) + 60;
			}
			if (varySize) { // agent size/radius
				people[i].size = (double) twistor.nextInt(11) / 10 * 0.1 + 0.20;
			}
			if (varySpeed) {// agent preferred speed
				people[i].speed = (double) twistor.nextInt(10) / 10 * 0.1 + 0.6;
			}
			// Initialises a random direction of travel for each agent.
			double angle = 2 * twistor.nextDouble() * Math.PI;
			// Initial speeds for each agent.
			people[i].vx = 0.1 * Math.cos(angle);
			people[i].vy = 0.1 * Math.sin(angle);
			// Initialise the distance travelled by each agent.
			// For each agent... 
			for (int j = i; j < N; j++) {
				// Matrix is symmetric.
				dists[i][j] = 0.0;
				dists[j][i] = 0.0;
			}
		}
	}

	// Sets random initial positions of agents (without any overlap).
	// Requires the following input:
	// 1. Index of the floor field used to assess any overlaps with initial placement.
	public void setInitial(int fieldIndex) { 
		//System.out.println("Setting random initial agent positions.");
		int a1, a2, a3, a4;
		// Set the initial values of the floor field in each cell.
		// For each row in the grid...
		for (int i = 0; i < gridSize; i++) {
			//...for each value in the row...
			for (int j = 0; j < gridSize; j++) {
				//...if the floor field = 0 at this cell...
				if (fstatic[fieldIndex][i][j] == 0) { 
					//...set temporary floor field to 0.
					floorField[i][j] = fstatic[fieldIndex][i][j];
				} else { // Otherwise set it to 1.
					floorField[i][j] = 1.0;
				}
			}
		}
		/* 
		Now find the average floor field over cell + nearest neighbours.
		This will identify the cells of the grid that are within an agent
		size of walls and obstacles.  They are consequently excluded from 
		agent placement otherwise, agents would overlap with walls/obstacles
		and be repelled with huge forces!
		*/
		// Run on the all cells within 4 cells of the obstacle...
		for (int uu = 0; uu < 4; uu++) {
			//...for each row of the grid...
			for (int i = 0; i < gridSize; i++) {
				//...for each column of the grid...
				for (int j = 0; j < gridSize; j++) {
					a1 = i - 1; // neighbour on the left
					// If the cell is at the left boundary already...
					if (a1 < 0) {
						//...then fix to 0.
						a1 = 0;
					}
					a2 = i + 1; // neighbour on the right
					// If the cell is at the right boundary already...
					if (a2 > gridSize - 1) {
						//...then fix to the grid size (Java is zero-indexed).
						a2 = gridSize - 1;
					}
					a3 = j - 1; // neighbour below
					// If the cell is at the bottom already...
					if (a3 < 0) {
						//...then fix to 0.
						a3 = 0;
					}
					a4 = j + 1; // neighbour above
					// If the cell is at the top already...
					if (a4 > gridSize - 1) {
						//...then fix to the grid size (Java is zero-indexed).
						a4 = gridSize - 1;
					}
					// Find the average floor field.
					double locAvFloorField = (floorField[i][j] + floorField[a1][j] + floorField[a2][j] + 
						floorField[i][a3] + floorField[i][a4]) / 5;
					// If average is < 1, (someone is 0)...
					if (locAvFloorField < 1) {
						//...then set floor field = 0.
						excluFloorField[i][j] = 0.0;
					} else { // Otherwise, set it = 1
						excluFloorField[i][j] = 1.0;
					}
				}
			}
			// Set wall/obstacle exclusion field.
			// For each row in the grid...
			for (int i = 0; i < gridSize; i++) {
				//...for each value in the row...
				for (int j = 0; j < gridSize; j++) {
					//...set the floor field value.
					floorField[i][j] = excluFloorField[i][j];
				}
			}
		}

		/* Assign the agents random positions and orientations (on grid for now, for
		simplicity). ensure agents don't overlap initially and are not sitting on the
		boundary.*/
		// For each agent...
		for (int i = 0; i < N; i++) {
			//...initialise candidate x/y-coordinates.
			int ranXCand, ranYCand;
			int cc = 0; // Entity placement counter.

			// Assign agent characteristics here:
			if (varyMass) { // agent mass in kg
				people[i].mass = (double) twistor.nextInt(41) + 60;
			}
			if (varySize) { // agent size/radius in metres
				people[i].size = (double) twistor.nextInt(11) / 10 * 0.1 + 0.20;
			}
			if (varySpeed) {// agent preferred speed in m/s
				people[i].speed = (double) twistor.nextInt(10) / 10 * 0.1 + 0.6;
			}
			// Randomly assign x and y coordinates
			// Must be cells away from the boundaries (i.e. free space cells).
			ranXCand = twistor.nextInt(gridSize - 10) + 5;
			ranYCand = twistor.nextInt(gridSize - 10) + 5;

			// Convert grid cell positions into position in metres.
			people[i].x = (double) ranXCand / 10;
			people[i].y = (double) ranYCand / 10;

			// Convert position back to obtain grid position array.
			getGridpos(i);
			cc = 0;
			/*
			If the assigned cell is within the exclusion field of either another
			agent or an obstacle...
			*/
			while (grid[ranXCand][ranYCand] != 0 || floorField[gridPos[0]][gridPos[1]] == 0.0) {
				//...re-assign intiial position.
				ranXCand = twistor.nextInt(gridSize - 10) + 5;
				ranYCand = twistor.nextInt(gridSize - 10) + 5;
				people[i].x = (double) ranXCand / 10; // grid cell size hard coding!
				people[i].y = (double) ranYCand / 10; // grid cell size hard coding!
				// Convert position back to obtain grid position array.
				getGridpos(i);
				// Increment placement counter
				cc += 1;
				// If you have tried to place agent 10000 times...
				if (cc > 10000) {
					//...then display this warning
					System.out.println(" ");
					System.out.println("***********************************");
					System.out.println("Cannot assign this many agents!");
					System.out.println("... shutting down ...");
					System.out.println("***********************************");
					System.exit(0);
				}
			}
			// Set up temporary exclusion field around newly-placed agent.
			/* For cells around the central cell of the agent (hard-coded so 
			that any possible agent size fits in - may want to change at some
			point.)...*/
			for (int uu = -4; uu < 5; uu++) {
				for (int vv = -4; vv < 5; vv++) {
					// Compute distance of cell from centre.
					double distFromCent = Math.sqrt((uu * uu) + (vv * vv));
					// If the cells or part of the cells are within the boundary...
					if (distFromCent <= 5 || (Math.sqrt(((Math.abs(uu)-1) * 
						(Math.abs(uu) - 1)) + ((Math.abs(vv) - 1) * 
						(Math.abs(vv) - 1))) < 5 && (distFromCent > 5))) {
						//...block them out.
						grid[ranXCand + uu][ranYCand + vv] = 1.0;
					}
				}
			}
			// Initialises a random direction of travel for each agent.
			double angle = 2 * twistor.nextDouble() * Math.PI;
			people[i].vx = 0.1 * Math.cos(angle);
			people[i].vy = 0.1 * Math.sin(angle);

			// Initialise the distance between each agent.
			// For each agent... 
			for (int j = i; j < N; j++) {
				//...matrix is symmetric.
				dists[i][j] = 0.0;
				dists[j][i] = 0.0;
			}
		}
	}

	// Get current grid position for agent chosen.
	// Requires the following input:
	// 1. Index of the agent chosen.
	public void getGridpos(int chosen) { 
		// Change agent's coordinates into corresponding grid cell.
		double gridXConv = people[chosen].x / size;
		double gridYConv = people[chosen].y / size;
		// Each grid cell is 10x10cm, and Java counts from 0.
		gridXConv = gridXConv * (size * 10 - 1); // grid cell size hard coding!
		gridYConv = gridYConv * (size * 10 - 1); // grid cell size hard coding!
		gridPos[0] = (int) (long) Math.round(gridXConv);
		gridPos[1] = (int) (long) Math.round(gridYConv);
	}

	// Get new grid position for agent chosen.
	// Requires the following input:
	// 1. Index of the agent chosen.
	public void getnewGridpos(int chosen) { 
		// Change agent's coordinates into corresponding grid cell.
		double gridXConv = people[chosen].xnew / size;
		double gridYConv = people[chosen].ynew / size;
		// Each grid cell is 10x10cm, and Java counts from 0.
		gridXConv = gridXConv * (size * 10 - 1); // grid cell size hard coding!
		gridYConv = gridYConv * (size * 10 - 1); // grid cell size hard coding!
		gridPos[0] = (int) (long) Math.round(gridXConv);
		gridPos[1] = (int) (long) Math.round(gridYConv);
	}

	// Get grid position for given x/y coordinate pair.
	// Requires the following inputs:
	// 1. X-coordinate.
	// 2. Y-coordinate.
	public void getposGridpos(double gridXConv, double gridYConv) { 
		// Change coordinates into corresponding grid cell.
		gridXConv = gridXConv / size;
		gridYConv = gridYConv / size;
		gridXConv = gridXConv * (size * 10 - 1); // grid cell size hard coding!
		gridYConv = gridYConv * (size * 10 - 1); // grid cell size hard coding!
		gridPos[0] = (int) (long) Math.round(gridXConv);
		gridPos[1] = (int) (long) Math.round(gridYConv);
	}

	// Get magnitude of a 2D vector.
	// Requires the following inputs:
	// 1. X-component of the vector.
	// 2. Y-component of the vector.
	public double magnitude(double x, double y) { 
		double out = Math.sqrt(x * x + y * y);
		return out;
	}

	// Implements Heaviside step function for a given value and threshold. 
	// =1 if >= threshold and =0 otherwise.
	// Requires the following inputs:
	// 1. Value to be assessed.
	// 2. Value at which Heaviside function changes.
	public double heaviside(double x, double threshold) { 
		// Initialise the output.
		double out = 0.0;
		// If the value is above the threshold...
		if (x >= threshold) {
			//...then the output is one.
			out = 1.0;
		}
		return out;
	}

	// Method to check if agents stand on the boundary.
	// Requires the following input:
	// 1. Index of the agent chosen.
	public boolean checkPos(int chosen) { 
		// How many cells to include up and down from central cell of agent.
		int ui, uj;
		// Initialise the grid cell positions of boundaries of agent and 
		// distance to move agents if overlap discovered.
		double entBoundXPos, entBoundYPos, dd;
		// Assume overlap with any other agents is false at first.
		boolean out = false;
		// Initialise the number of agents that overlaop and the amount by 
		// which they overlap as 0.
		numEntOverlap = 0.0;
		entOverlap[0] = 0.0;
		entOverlap[1] = 0.0;

		// For cells to left/right of centre...
		for (ui = -1; ui < 2; ui++) {
			//...for cells up/down from the current cell...
			for (uj = -1; uj < 2; uj++) {
				//...if the cell is diagonally adjacent to agent position...
				if ((Math.abs(ui) + Math.abs(uj)) == 2) {
					//...the position of boundary of agent is divided by sqrt(2) as cell 
					// is nearest diagonal neighbour.
					entBoundXPos = people[chosen].x + ui * people[chosen].size / Math.sqrt(2);
					entBoundYPos = people[chosen].y + uj * people[chosen].size / Math.sqrt(2);
				} else { // Otherwise, for the cells directly adjacent...
					//...the boundary is an integer multiple of the agent radius.
					entBoundXPos = people[chosen].x + ui * people[chosen].size;
					entBoundYPos = people[chosen].y + uj * people[chosen].size;
				}
				// Convert position of agent boundary to the corresponding cell.
				getposGridpos(entBoundXPos, entBoundYPos);
				// If the agent is overlapping with an obstacle...
				try {
					if (fstatic[people[chosen].destination][gridPos[0]][gridPos[1]] == 0) {
						//...then out is true... 
						out = true;
						//...Increase the number of agents currently overlapping.
						numEntOverlap += 1;
						/* 
						Calculate the x- and y-components of the amount by which
						agent overlaps.
						*/
						entOverlap[0] += people[chosen].x - entBoundXPos;
						entOverlap[1] += people[chosen].y - entBoundYPos;
					}
				} catch (ArrayIndexOutOfBoundsException e) {
					// If agent ends up in impossible position, then let user know.
					System.out.println("Entity " + chosen + " is at grid position: " 
					+ gridPos[0] + ", " + gridPos[1]);
					System.out.println("It's proposed coordinates are: (" + 
					entBoundXPos + ", " + entBoundYPos + ").");
					System.out.println("It's current coordinates are: (" + 
					people[chosen].x + ", " + people[chosen].y + ").");
					System.out.println("The agent has a radius of " + 
					people[chosen].size);
				}
			}
		}
		// If any agent does overlap with an obstacle...
		if (numEntOverlap > 0) {
			//...calculate the amount by which agent overlaps.
			dd = Math.sqrt(entOverlap[0] * entOverlap[0] + entOverlap[1] * entOverlap[1]);
			// Normalise the amount of overlap.
			entOverlap[0] = entOverlap[0] / dd;
			entOverlap[1] = entOverlap[1] / dd;
		}
		// Returns true if any agent overlaps and false otherwise.
		return out;
	}

	/* 
	Find direction of highest positive and/or lowest negative gradient of
	floor field at agent's current position.
	Requires the following input:
		1. Index of the agent chosen.
	*/
	public void getGradient(int chosen) {
		// Indices for agent x/y cells and x/y cells outside of agent, respectively.
		int i = 0, j = 0, k = 0, l = 0;
		// Distances of cells outside the agent boundary from the agent centre.
		double gradVecs1 = 0.0, gradVecs2 = 0.0;
		/* Entity movement direction, distance of cell from central agent cell, 
		net floor field gradient around agent, noise in gradient detection by
		agent.*/
		double entDir, cellDist, totGrad, gradNoise;
		double dirx, diry;
		// Unnormalised x and y directions of the agent.
		dirx = people[chosen].vx;
		diry = people[chosen].vy;
		// Magnitude of direction vector.
		entDir = Math.sqrt(dirx * dirx + diry * diry);
		// Normalise directions.
		dirx = dirx / entDir;
		diry = diry / entDir;
		// Convert agent's position into a grid cell.
		getGridpos(chosen);
		// Initialise x/y components of the floor field gradient.
		gradient[0] = 0.0;
		gradient[1] = 0.0;

		/*
		In a circle of grid cells of radius R, centred around the cell 
		of the agent, with i moving along the horizontal axis and j
		moving along the vertical.
		*/
		for (i = 0; i < (R + R + 1); i++) {
			for (j = 0; j < (R + R + 1); j++) {
				/* 
				As long as i and j are not on the agent's cells 
				and are not both = R...
				*/
				if ((i != R && j != R) || (i == R && j != R) || (i != R && j == R)) {
					// Calculate the position of the cell from the agent's cell.
					k = gridPos[0] + i - R;
					l = gridPos[1] + j - R;
					/* 
					Find the x/y components of the distance of the
					cell from the agent cell, find the Euclidean distance, then
					normalise the x/y components with it.
					*/
					gradVecs1 = (double) (k - gridPos[0]);
					gradVecs2 = (double) (l - gridPos[1]);
					cellDist = magnitude(gradVecs1, gradVecs2);
					gradVecs1 = gradVecs1 / cellDist;
					gradVecs2 = gradVecs2 / cellDist;

					// How to deal with boundaries. No periodic boundaries!
					if (k < 0) { // Negative x-index
						//...set to 0.
						k = 0;
					}
					// x-index exceeds grid size. -1 because Java counts from 0.
					if (k > (gridSize - 1)) {
						//...set to grid size.
						k = gridSize - 1;
					}
					if (l < 0) { // Negative y-index
						//...set to 0.
						l = 0;
					}
					// y-index exceeds grid size. -1 because Java counts from 0.
					if (l > (gridSize - 1)) {
						//...set to grid size.
						l = gridSize - 1;
					}

					/* 
					Use this to implement a field of view for the gradient
					detection in the dynamic floor field. Only consider values
					in front of agent.
					*/
					double grad = fstatic[people[chosen].destination][k][l] - 
						fstatic[people[chosen].destination][gridPos[0]][gridPos[1]];

					// Increase the x/y components of the gradient by 
					gradient[0] += gradVecs1 * grad;
					gradient[1] += gradVecs2 * grad;
				}
			}
		}

		// Find total floor field gradient for agent.
		totGrad = magnitude(gradient[0], gradient[1]);

		// If total floor field gradient = 0...
		if (totGrad == 0) {
			//...then randomly generate a gradient
			totGrad = Math.atan2(people[chosen].vy, people[chosen].vx)
					+ 2 * noise * Math.PI * (2 * twistor.nextDouble() - 1);
			// Set totGrad to be in the interval [0, 2*pi].
			if (totGrad > 2 * Math.PI) {
				totGrad -= 2 * Math.PI;
			}
			if (totGrad < 0) {
				totGrad += 2 * Math.PI;
			}
			// Calculate x/y components of the gradient vector
			gradient[0] = Math.cos(totGrad);
			gradient[1] = Math.sin(totGrad);
		} else { // Otherwise, if the total floor field gradient is not zero...
			//...normalise floor field gradient.
			gradient[0] = gradient[0] / totGrad;
			gradient[1] = gradient[1] / totGrad;

			// Add some noise to the gradient detection to avoid individual agents getting
			// stuck in doors.
			gradNoise = Math.atan2(gradient[1], gradient[0]) + noise * Math.PI * 
			(2 * twistor.nextDouble() - 1);
			// Set noise to be in the interval [0, 2*pi].
			if (gradNoise > 2 * Math.PI) {
				gradNoise -= 2 * Math.PI;
			}
			if (gradNoise < 0) {
				gradNoise += 2 * Math.PI;
			}
			gradient[0] = Math.cos(gradNoise);
			gradient[1] = Math.sin(gradNoise);
		}
	}

	// This method updates the movement of all agents.
	// Requires the following input:
	// 1. Number indicating the current timestep.
	public void updateMovement(int timestep){ 
		//System.out.println("Moving all agents now.");
		// Initialise navigation and repulsive social forces.
		double fnavx = 0.0, fnavy = 0.0, frepx = 0.0; 
		double frepy = 0.0, frepwx = 0.0, frepwy = 0.0;
		// Initialise array for destination occupancies.
		destPopularity = new int [numDest];
		// Update to next timestep.
		time += deltaT;
		
		// For every agent...
		for (int i = 0; i < N; i++) {
			//...between every agent and this agent, including itself...
			for (int j = i; j < N; j++) { 
				dists[i][j] = size;
				//...calculate distance between agents as Euclidean distance.
				dists[i][j] = magnitude(people[i].x - people[j].x, people[i].y - people[j].y);
				// Distance between j and i is the same as between i and j.
				dists[j][i] = dists[i][j]; 
			}
			// The distance between an agent and itself is just its size.
			dists[i][i] = size;

			// body interaction force (helbing2000)
			frepx = 0.0;
			frepy = 0.0;
			// Total force acting on agent.
			//bodyForces[i] = 0.0;

			/* Now, to save a bit of computation time, only calculate the social forces 
			between this agent and other agents within 2m of it.  Threshold distance 
			chosen arbitrarily.*/
			// Initialise ArrayList to store distances between this agent and all others.
			ArrayList<Integer> close = new ArrayList<Integer>();
			// For each other agent...
			for (int u = 0; u < dists[i].length; u++) {
				//...if the distance is less than 2m...
				if (dists[i][u] <= 2) {
					//...then add the agent to the list.
					close.add(u);
				}
			}
			// Convert mutable ArrayList to immutable array.
			Integer[] closest = close.toArray(new Integer[0]);

			// For each close agent to this agent...
			for (int k = 0; k < closest.length; k++) {
				//...calculate the total repulsive force due to other agents.
				int ent = closest[k];
				// Only consider other agents...
				if (ent != i) {
					/*
					This would be where to go to fix ridiculously high
					repulsive forces between agents.
					*/
					//...calculate repulsive force according to Helbing et al. 2000.
					double srep = people[i].A
							* Math.exp(((people[i].size + people[ent].size)
								- dists[i][ent]) / people[i].B);
					// Initialise the components of repulsive forces due to agents touching.
					double repcomp = 0.0, reptang = 0.0;
					// Normalised x- and y-distances between i and k.
					double xr = (people[i].x - people[ent].x)
							/ magnitude((people[i].x - people[ent].x), 
								(people[i].y - people[ent].y));
					double yr = (people[i].y - people[ent].y)
							/ magnitude((people[i].x - people[ent].x), 
								(people[i].y - people[ent].y));

					// If pedestrians touch each other...
					if (dists[i][ent] < (people[i].size + people[ent].size)) {
						//...then there is also sliding friction.
						repcomp = kpara * (people[i].size + people[ent].size
							- dists[i][ent]);
						/*
						Cross product of difference in agent speed and 
						distance between them.
						*/
						double Deltav = (people[ent].vx - people[i].vx) * (-yr)
							+ (people[ent].vy - people[i].vy) * (xr);
						// Tangential component of sliding friction.
						reptang = kappa * (people[i].size + people[ent].size 
							- dists[i][ent]) * Deltav;
					}
					// Repulsive force components. 0 if agents not touching.
					frepx += (srep + repcomp) * xr + (reptang) * (-yr);
					frepy += (srep + repcomp) * yr + (reptang) * (xr);
					// Add the repulsive component acting away from the agent to the 
					//total touching repulsive force.
					//bodyForces[i] += repcomp;
				}
			}

			// No explicit interaction with walls. included implicitly via the static 
			// floor field.
			// Initialise the wall repulsive force components.
			frepwx = 0.0;
			frepwy = 0.0;
			// Interaction with walls only if pedestrian touches it...
			if (checkPos(i)) {
				//...calculate x/y components of wall-agent force.
				frepwx = kpara * (numEntOverlap * 2 * people[i].size / 9) * entOverlap[0]
						- kappa * (numEntOverlap * 2 * people[i].size / 9)
								* (people[i].vx * (-entOverlap[1]) + 
								people[i].vy * entOverlap[0]) * (-entOverlap[1]);
				frepwy = kpara * (numEntOverlap * 2 * people[i].size / 9) * entOverlap[1]
						- kappa * (numEntOverlap * 2 * people[i].size / 9)
								* (people[i].vx * (-entOverlap[1]) + 
								people[i].vy * entOverlap[0]) * (entOverlap[0]);
				// Add this force to the total touching repuslive force experienced 
				// by the agent,
				//bodyForces[i] += kpara * (numEntOverlap * 2 * people[i].size / 9);
			}

			// Find the preferred movement direction acceleration:
			getGradient(i);
			// Calculate the components of the driving force.
			fnavx = (people[i].speed * gradient[0] - people[i].vx) / tau;
			fnavy = (people[i].speed * gradient[1] - people[i].vy) / tau;

			// Calculate the agent's new velocity components due to the combination 
			// of all forces acting on agent:
			people[i].vxnew = people[i].vx + deltaT * (frepx / 
				people[i].mass + fnavx + frepwx / people[i].mass);
			people[i].vynew = people[i].vy + deltaT * (frepy / 
				people[i].mass + fnavy + frepwy / people[i].mass);

			// Calculate new x/y position of agent.
			people[i].xnew = people[i].x + deltaT * people[i].vxnew;
			people[i].ynew = people[i].y + deltaT * people[i].vynew;
			}
		
		/* Now that forces are calculated, move each agent, assign their new 
		speeds, and assign new destinations where neccesary.*/
		
		// For each agent...
		for (int i = 0; i < N; i++) {
			//...find their new grid position.
			getnewGridpos(i);

			// Update position and speed.
			people[i].x = people[i].xnew;
			people[i].y = people[i].ynew;
			people[i].vx = people[i].vxnew;
			people[i].vy = people[i].vynew;
			// Add the total touching forces felt by this agent at this timestep to 
			// the current total force experienced.
			// For each destination...
			for (int u = 0; u < numDest; u++) {
				//...determine the distance of the centre of mass of the agent from 
				// the destination.		
				double dd = distances[u][gridPos[0]][gridPos[1]];
				// If agent is within threshold distance of destination...
				if (dd <= destSize) { // Threshold can vary!
					//...then it is counted for occupancy.
					destPopularity[u]++;
					/* Could also put in below if statement if you want to 
					only consider agents wanting to visit that destination.
					*/
				}
			}
		}
	}

	/* Function which updates the choices of all agents (if necessary) using the
	specified destination choice model.  Also calculates and keeps track of the 
	waiting times of agents at destinations.
	Requires the following input:
	1. Number indicating the current timestep.
	*/
	public void updateChoices(int timestep) {
		// For each agent...
		for (int i = 0; i < N; i++) {
			//...find their new grid position.
			getGridpos(i);
			if (people[i].tStay > 0) {
				/* For agents currently at the destination, decrement the 
				stay time at each step.*/
				people[i].tStay--;
			}
			// For each destination...
			for (int u = 0; u < numDest; u++) {
				//...calculate the distance between agent grid position and destination position.	
				double dd = distances[u][gridPos[0]][gridPos[1]];
				// If agent reaches its next destination...
				if (dd <= destSize && u == people[i].destination) {
					//...if the agent has just arrived...
					if (people[i].tStay == -1) {
						//...then assign it a time to spend at destination.
						// Translation factor can be changed to suit intuition.
						people[i].tStay = 400 + getNextIntExpDistSampling(0.003); 
					}
					// If agent has spent the required time at this destination....
					if (people[i].tStay == 0) {
						//...then update the agent's schedule accordingly.
						// First, check if the current activity is in the schedule.
						int rmActInd = people[i].schedule.indexOf(people[i].currAct);
						// If the activity is present in the schedule...
						if (rmActInd != -1) {
							//...find and remove it.
							char rmAct = people[i].schedule.get(rmActInd);
							people[i].updateSchedule(rmAct);
						} else { // Otherwise, if the activity is not present in the schedule...
							//...do not change the schedule and add to the schedule history 
							// of the agent.
							people[i].schedHist.add(people[i].schedule.toArray(new Character[0]));
						}
						// If the Markov model is included...
						if (transMat != null) {
							/*...then assign new destination according to the 
							model with Markov.
							This is the line to change the destination choice model used.
							*/
							people[i].destination = fullModel(people[i], occParam, 
							distParam, desParam, transMat, destPopularity, 
							destActs, distances, gridPos, newNextDest, expParam);
						}
						// Otherwise, if the Markov model is not included...
						else {
							//...then use the model without Markov.
							// This is the line to change the destination choice model used.
							people[i].destination = fullModel(people[i], occParam, 
							distParam, desParam, destPopularity, destActs, 
							distances, gridPos, newNextDest, expParam);
						}

						// Add the new destination to the list of destinations chosen.
						people[i].dests.add(people[i].destination);
						// Add the timestep at which the decision was made.
						people[i].decisionTimes.add(timestep);
						// Reset the agent's staying time for the next destination.
						people[i].tStay = -1;
					}
				}
			}
		}
	}

	/*
	Random destination choice model function.  
	Randomly choose a destination from those available.
	Requires the following input:
	1. The number of destinations present in the simulation.
	*/
	public int ranDest(int numDest) {
		// New random number generator for...
		Random kerplunk = new Random();
		//...randomly choosing a destination.
		int dest = kerplunk.nextInt(numDest);
		return dest;
	}

	/*
	Markov destination choice model function.  Select new destination via 
	Markov model alone using given transition matrix.
	Requires the following inputs:
	1. Matrix of transition probabilities between each possible pair of destinations.
	2. The index of the current destination of the agent.
	3. The number of possible destinations in the simulation.
	*/
	public int markovDest(double [][] transMatrix, int currentDest, int numDest) {
		// New random number generator for choosing a new destination.
		Random guessWho = new Random();
		// Array for collecting the cumulative sum of transition probabilities.
		double [] cumSum = new double[numDest];
		// Initialise the next destination.
		int nextDest = 0;

		/* The first entry in the cumulative sum is the first value of the 
		current destination row of the transition matrix.*/
		cumSum[0] = transMatrix[currentDest][0];
		// For each destination...
		for (int u = 1; u < numDest; u++) {
			//...add the transition probability to the cumulative sum and make that
			// a new value in the cumulative sum array.
			cumSum[u] = cumSum[u - 1] + transMatrix[currentDest][u];
		}

		// Generate a random number between 0-1...
		double selection = guessWho.nextDouble();
		/*...and where it sits in the cumulative entries determines the next 
		destination such that if the cumsum[dest] <= selection < cumsum[DEST], 
		then DEST is next destination.*/
		for (int f = 0; f < numDest; f++) {
			if (selection < cumSum[f]) {
				nextDest = f;
				break;
			}
		}
		return nextDest;
	}

	/*
	Convert transition matrix entries into exponential form for destination choice
	models in exponential form that also include a Markov model.
	Requires the following input:
	1. Matrix of transition probabilities between each possible pair of destinations.
	*/
	public double [][] markovMatToExp(double [][] transMatrix) {
		// Initialise exponent matrix with same dimensions as transition matrix.
		double [][] betas = new double [transMatrix.length][transMatrix[0].length];
		// For each column in the transition matrix...
		for (int h = 0; h < transMatrix[0].length; h++) {
			//...for each value in the column...
			for (int s = 0; s < transMatrix.length; s++) {
				//...convert transition matrix element.
				betas[h][s] = Math.log(transMatrix[h][s]);
			}
		}
		return betas;
	}

	/* Select new destination via Markov model without a predefined transition matrix.
	Requires the following inputs:
	1. The index of the current destination of the agent.
	2. The number of possible destinations in the simulation.
	*/
	public int markovDest(int currentDest, int numDest) {
		// New random number generator for choosing a new destination.
		Random battleship = new Random();
		// Array for collecting the cumulative sum of transition probabilities.
		double [] cumSum = new double[numDest];
		// Initialise the next destination.
		int nextDest = 0;
		// Initialise transition matrix.
		transMat = new double[numDest][numDest];
		// Normalisation constant for elements of transition matrix.
		double normConst = 0;
		// For each row in the transition matrix...
		for (int r = 0; r < numDest; r++) {
			//...start normalisation constant.
			normConst = 0;
			// For each value in the row...
			for (int c = 0; c < numDest; c++) {
				//...if the column index is greater than the row index...
				if (c >= r) {
					//...randomly generate a transition probability...
					transMat[r][c] = twistor.nextDouble();
					//...and duplicate to ensure symmetry.
					transMat[c][r] = transMat[r][c];
				}
				// Increment normalisation constant by the element.
				normConst += transMat[r][c];
			}
			// For each row...
			for (int c = 0; c < numDest; c++) {
				//...divide all constants by the normalisation constant for that row.
				transMat[r][c] /= normConst;
			}
		}

		// Now choose the next destination using the freshly-made transition matrix.

		/* The first entry in the cumulative sum is the first value of the 
		current destination row of the transition matrix.*/
		cumSum[0] = transMat[currentDest][0];
		// For each destination...
		for (int u = 1; u < numDest; u++) {
			//...add the transition probability to the cumulative sum and make that
			// a new value in the cumulative sum array.
			cumSum[u] = cumSum[u - 1] + transMat[currentDest][u];
		}

		// Generate a random number between 0-1...
		double selection = battleship.nextDouble();
		/*...and where it sits in the cumulative entries determines the next 
		destination such that if the cumsum[dest] <= selection < cumsum[DEST], 
		then DEST is next destination.*/
		for (int f = 0; f < numDest; f++) {
			if (selection < cumSum[f]) {
				nextDest = f;
				break;
			}
		}
		return nextDest;
	}

	/*
	Choose the next destination using an occupancy-only multinomial logit model.
	Requires the following inputs:
	1. The number of people (occupancy) at each destination.
	2. The value of the occupancy parameter.
	*/
	public int busyDest(int[] numPeople, double param) {
		// Initialise the choice probability array.
		double [] probChoose = new double[numPeople.length];
		// Initialise the next destination.
		int nextDest = 0;

		// Create array of expontial probabilities according to occupancy.
		double [] exponent = busyExp(numPeople, param);
		// For each exponential value...
		for (int u = 0; u < exponent.length; u++) {
			//...calculate the choice probability for each destination.
			probChoose[u] = Math.exp(exponent[u]);
		}
		// Assign next destination.
		nextDest = nextDest(probChoose);

		return nextDest;
	}

	/*
	Generate the occupancy exponents for a multinomial logit model containing 
	occupancy as a predictor.
	Requires the following inputs:
	1. The number of people (occupancy) at each destination.
	2. The value of the occupancy parameter.
	*/
	public double [] busyExp(int[] numPeople, double param) {
		// Create array of expontial probabilities according to occupancy.
		double [] exponent = new double[numPeople.length];
		// For each destination...
		for (int q = 0; q < numPeople.length; q++) {
			//...calculate the busyness part of the choice probability for that 
			// destination.
			exponent[q] = param * numPeople[q];
		}
		return exponent;
	}

	/* Generate the occupancy exponents normalised by the highest occupancy 
	observed at a given timestep.
	Requires the following inputs:
	1. The number of people (occupancy) at each destination.
	2. The value of the occupancy parameter.
	*/
	public double [] scaledBusyExp(int[] numPeople, double param) {
		// Create array of expontial probabilities according to occupancy.
		double [] exponent = new double[numPeople.length];
		// Create array of normalised occupancies at each destination.
		double [] scaledNumPeople = new double[numPeople.length];
		// Create a copy of the occupancy array that can be sorted to find the 
		// maximum occupancy.
		int [] numPeopleCopy = new int[numPeople.length];
		// For each destination...
		for (int x = 0; x < numPeople.length; x++) {
			//...create a copy of its occupancy.
			numPeopleCopy[x] = numPeople[x];
		}
		// Sort the occupancies into ascending order...
		Arrays.sort(numPeopleCopy);
		//...to find the largest.
		double maxOcc = numPeopleCopy[(numDest - 1)];
		// For each destination...
		for (int q = 0; q < numPeople.length; q++) {
			//...normalise the occupancy.
			scaledNumPeople[q] = numPeople[q] / maxOcc;
			// Calculate the busyness part of the choice probability for that 
			// destination.
			exponent[q] = param * scaledNumPeople[q];
		}
		return exponent;
	}

	/*
	Choose the next destination using an distance-only multinomial logit model.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The current destination of said agent.
	3. The real spatial coordinates of the agent.
	4. The distances of each grid cell for all destinations.
	5. The value of the distance parameter.
	*/
	public int dist_only (agent person, int curDest, int[] curPos, 
		double[][][] destDist, double distParam) {
		// Initialise the next destination.
		int nextDest = 0;
		// Initialise the choice probability array.
		double[] probChoose = new double[numDest];
		// Create array of expontial probabilities according to distance.
		double [] exponent = new double[numDest];
		// Calculate the distance exponents.
		exponent = distExp(person, curPos, destDist, distParam);
		// For each destination...
		for (int u = 0; u < exponent.length; u++) {
			//...calculate the probability of choosing it.
			probChoose[u] = Math.exp(exponent[u]);
		}
		// Assign next destination.
		nextDest = nextDest(probChoose);
		return nextDest;
	}

	/*
	Generate the distance exponents for a multinomial logit model containing 
	distance as a predictor.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The real spatial coordinates of the agent.
	3. The distances of each grid cell for all destinations.
	4. The value of the distance parameter.
	*/
	public double [] distExp (agent person, int[] curPos, double[][][] destDist, 
	double distParam) {
		// Total number of destinations.
		int numDest = destDist.length;
		// Create array of expontial probabilities according to distance.
		double[] exponent = new double[numDest];
		// Initialise the x/y distances of each destination from a given position.
		ArrayList<Double> dist = new ArrayList<Double>();
		// Distance from current position to a destination.
		double di = 0;
		// For each destination...
		for (int d = 0; d < numDest; d++) {
			//...find distance from agent's current position to destination...
			di = destDist[d][curPos[0]][curPos[1]];
			//...use said distance to calculate the exponent...
			exponent[d] = distParam * di;
			//...and add said distance to the destination distances
			dist.add(di);
		}
		// Add destination distances to the agent's list of decision distances.
		person.dists.add(dist);
		// Return distance exponents.
		return exponent;
	}

	/* Generate the distance exponents normalised by the highest distance 
	observed at a given timestep.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The real spatial coordinates of the agent.
	3. The distances of each grid cell for all destinations.
	4. The value of the distance parameter.
	*/
	public double [] scaledDistExp(agent person, int[] curPos, double[][][] destDist, double distParam) {
		// Total number of destinations.
		int numDest = destDist.length;
		// Create array of expontial probabilities according to distance.
		double[] exponent = new double[numDest];
		// Initialise the x/y distances of each destination from a given position.
		ArrayList<Double> dist = new ArrayList<Double>();
		// Distance from current position to a destination.
		double di = 0;
		// For each destination...
		for (int d = 0; d < numDest; d++) {
			//...find distance from agent's current position to destination...
			di = destDist[d][curPos[0]][curPos[1]];
			//...and add said distance to the destination distances
			dist.add(di);
		}
		// To find the largest destination distance, first create a mutable copy of 
		// the distance array.
		ArrayList<Double> distCopy = new ArrayList<Double>();
		// For each destination...
		for (int x = 0; x < dist.size(); x++) {
			//...add its distance from the agent's current position to the array.
			distCopy.add(dist.get(x));
		}
		
		// Sort the array into ascending order...
		Collections.sort(distCopy);
		//...and find the largest distance.
		double maxDist = distCopy.get((numDest - 1));
		// Now make a mutable array of normalised destination distances.
		ArrayList<Double> scaledDist = new ArrayList<Double>();
		// For each destination...
		for (int w = 0; w < dist.size(); w++) {
			//...normalise its distance.
			scaledDist.add(dist.get(w) / maxDist);
			// Use said distance to calculate the exponent.
			exponent[w] = distParam * scaledDist.get(w);
		}
		
		// Add destination distances to the agent's list of decision distances.
		person.dists.add(dist);
		// Return distance exponents.
		return exponent;
	}

	/*
	Choose the next destination using an desirability-only multinomial logit model.  
	Desirability depends on the agent's schedule and the activities available for 
	each destination.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The current schedule of activities of said agent.
	3. A matrix of every activity available at each destination.  Currently assumes 
	that all destinations have the same number of possible activities.
	4. The value of the desirability parameter.
	*/
	public int [] des (agent person, ArrayList<Character> entSched, char[][] destActs, 
		double priorityParam) {
		// Initialise the next destination.
		int nextDes = 0;
		// Total number of destinations.
		int numDest = destActs.length;
		// Create array of expontial probabilities.
		double [] exponent = new double[numDest];
		// Initialise the array of desirabilities of destinations.
		double [] destPriority;
		// Initialise the array of possible desirability values.
		double [] priority;
		// Initialise the choice probability array.
		double [] probChoose = new double[numDest];
		// This will store the output of the desExp function.
		double [][] output;

		// Calculate the desirbaility term of the destination choice probability.
		output = desExp(person, entSched, destActs, priorityParam, 1);
		// First entry in the output is the desirability exponent term.
		exponent = output[0];
		// Second entry is the desirability value assigned to each destination.
		destPriority = output[1];
		// Third entry is the list of all possible desirability values.
		priority = output[2];
		// Calculate the destination choice probability based only on desirability.
		for (int u = 0; u < exponent.length; u++) {
			probChoose[u] = Math.exp(exponent[u]);
		}
		// Assign next destination.
		nextDes = nextDest(probChoose);
		// Now that a destination is chosen, need to set the next activity.
		int nextAct = 0;
		// For each desirability value...
		for (int i = 0; i < priority.length; i++) {
			//...does the desirability of the chosen destination match?
			if (destPriority[nextDes] == priority[i]) {
				// If so, then we have found our next activity.
				nextAct = i;
			}
		}
		// Collect up both next destination and next activity as function output.
		int [] result = {nextDes, nextAct};
		return result;
	}

	/*
	Generate the desirability exponents for a multinomial logit model containing 
	desirability as a predictor.  Desirability depends on the agent's schedule 
	and the activities available for each destination.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The current schedule of activities of said agent.
	3. A matrix of every activity available at each destination.  Currently assumes 
	that all destinations have the same number of possible activities.
	4. The value of the desirability parameter.
	5. The value of the parameter of the negative exponential used to assign 
	desirability.
	*/
	public double [][] desExp (agent person, ArrayList<Character> entSched, char[][] destActs, 
		double priorityParam, double expParam) {
		// Total number of destinations.
		int numDest = destActs.length;
		// Initialise the array of possible desirability values.
		double [] priority = new double[entSched.size()];
		// Initialise the array of desirabilities of destinations.
		double [] destPriority = new double[numDest];
		// Mutable copy of the destination desirabilities.
		ArrayList<Double> destPriorities = new ArrayList<Double>();
		// Create array of expontial probabilities.
		double [] exponent = new double[numDest];
		// This will store the output of the function.
		double [][] out = new double[3][numDest];

		// For each activity in an agent's schedule...
		for (int i = 0; i < entSched.size(); i++) {
			//...activity desirability is a -ve exponential of position in schedule.
			priority[i] = Math.exp(expParam * i);
		} 
		/* Now to assign desirability values to destinations based on the activities 
		possible at each destination.*/
		// For each destination...
		for (int j = 0; j < numDest; j++) {
			//...this destination has not been assigned a desirability yet.
			boolean assigned = false;
			// For each activity in the agent schedule...
			for(int s = 0; s < entSched.size(); s++){
				//...for each possible activity at the destination...
				for (int t = 0; t < destActs[j].length; t++) {
					//...if the sth schedule activity matches the destination activity...
					if (entSched.get(s) == destActs[j][t]){
						//...then destination is assigned the sth desirability value.
						destPriority[j] = priority[s];
						assigned = true;
						break; // Stop looking through destination activities
					}
				}
				// If destination has a desirability...
				if (assigned) {
					//..., stop looking through schedule
					break;
				}
			}
			// If the destination cannot perform any scheduled activities...
			if (!assigned) {
				//...assign it a really low desirability.
				destPriority[j] = 1E-121;
			}
			// Now calculate the desireability term of the choice probability.
			exponent[j] = priorityParam * destPriority[j];
			destPriorities.add(destPriority[j]);
		}

		person.desires.add(destPriorities);

		// Collect up function output.
		// First entry in the output is the desirability exponent term.
		out[0] = exponent;
		// Second entry is the desirability value assigned to each destination.
		out[1] = destPriority;
		// Third entry is the list of all possible desirability values.
		out[2] = priority;
		return out;
	}

	/* Generate the distance exponents normalised by the highest distance 
	observed at a given timestep.  Desirability depends on the agent's schedule 
	and the activities available for each destination.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. The current schedule of activities of said agent.
	3. A matrix of every activity available at each destination.  Currently assumes 
	that all destinations have the same number of possible activities.
	4. The value of the desirability parameter.
	5. The value of the parameter of the negative exponential used to assign 
	desirability.
	*/
	public double [][] scaledDesExp (agent person, ArrayList<Character> entSched, char[][] destActs, 
		double priorityParam, double expParam) {
		// Total number of destinations.
		int numDest = destActs.length;
		// Initialise the array of possible desirability values.
		double [] priority = new double[entSched.size()];
		// Initialise the array of desirabilities of destinations.
		double [] destPriority = new double[numDest];
		// Mutable copy of the destination desirabilities.
		ArrayList<Double> destPriorities = new ArrayList<Double>();
		// The array of normalised diestination desirabilities.
		double [] scaledDestPriority = new double[numDest];
		// Create array of expontial probabilities.
		double [] exponent = new double[numDest];
		// This will store the output of the function.
		double [][] out = new double[3][numDest];

		// For each activity in an agent's schedule...
		for (int i = 0; i < entSched.size(); i++) {
			// Activity desirability is a -ve exponential of position in schedule.
			priority[i] = Math.exp(expParam * i);
		} 

		/* Now to assign desirability values to destinations based on the activities 
		possible at each destination.*/
		// For each destination...
		for (int j = 0; j < numDest; j++) {
			//...this destination has not been assigned a desirability yet.
			boolean assigned = false;
			// For each activity in the agent schedule...
			for(int s = 0; s < entSched.size(); s++){
				//...for each possible activity at the destination...
				for (int t = 0; t < destActs[j].length; t++) {
					//...if the sth schedule activity matches the destination activity...
					if (entSched.get(s) == destActs[j][t]){
						//...then destination is assigned the sth desirability value.
						destPriority[j] = priority[s];
						assigned = true;
						break; // Stop looking through destination activities
					}
				}
				// If destination has a desirability...
				if (assigned) {
					//...stop looking through schedule
					break;
				}
			}
			// If the destination cannot perform any scheduled activities...
			if (!assigned) {
				//...assign it a really low desirability.
				destPriority[j] = 1E-121;
			}
			//...record the desirabilities of the destination.
			destPriorities.add(destPriority[j]);			
		}
		// Record the desirabilities of all destinations for this choice.
		person.desires.add(destPriorities);

		// Now rescale the desireability.  First make a copy of the desirability array.
		double[] destPriorityCopy = new double[numDest];
		// For each destination...
		for (int x = 0; x < destPriority.length; x++) {
			//...copy its desirability.
			destPriorityCopy[x] = destPriority[x];
		}
		// Sort the desirabilities of all destinations into ascending order...
		Arrays.sort(destPriorityCopy);
		//...and find the largest.
		double maxDesire = destPriorityCopy[(numDest - 1)];
		// Now rescale each desirability.
		for (int h = 0; h < destPriority.length; h++) {
			scaledDestPriority[h] = destPriority[h] / maxDesire;
		}
		for (int j = 0; j < scaledDestPriority.length; j++) {
			// Now calculate the desireability term of the choice probability.
			exponent[j] = priorityParam * scaledDestPriority[j];
		}
		
		// Collect up function output.
		// First entry in the output is the desirability exponent term.
		out[0] = exponent;
		// Second entry is the desirability value assigned to each destination.
		out[1] = destPriority;
		// Third entry is the list of all possible desirability values.
		out[2] = priority;
		return out;
	}

	/* 
	Determine the next destination for an agent using a multinomial logit model 
	with Markov included.  This model includes occupancy of destinations, the 
	distance of the agent from each destination, and the desirability of the 
	destination based on the agent's schedule and the activities possible at each 
	destination.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. Value of the occupancy parameter.
	3. Value of the distance parameter.
	4. Value of the desirability parameter.
	5. Matrix of transition probabilities between every pair of destinations for 
	the Markov model.
	6. Array of occupancies for all destinations.
	7. Matrix of available activities at each destination.  Assumes that each 
	destination has the same number of available activities.
	8. An array of matrices giving the distance of each spatial grid cell from 
	every destination.
	9. The spatial grid position of the agent.
	10. Is the new next destination constraint being enforced?
	11. The parameter of the negative exponential function used to calculate 
	desirabilities.
	*/
	public int fullModel (agent person, double busyParam, double distParam, 
	double desParam, double [][] transMat, int [] destOccs, char [][] destActs,
	double [][][] dist, int [] currPos, boolean nextDestConstr, double expParam) {
		// Initialise the next destination.
		int nextDest = 0;
		// Total number of destinations.
		int numDest = destOccs.length;
		// Pull in the agent's current schedule.
		ArrayList<Character> entSched = person.schedule;
		// Pull in the agent's current destination.
		int currDest = person.destination;
		// Create array of expontial cost functions for each destination.
		double [] exponent = new double [numDest];
		// Create corresponding array of destination choice probabilities.
		double [] probabilities = new double [numDest];

		// Calculate each term of the cost function for each destination.
		// Occupancy term.
		double [] busyExp = busyExp(destOccs, busyParam); 
		// Distance term.
		double [] distExp = distExp(person, currPos, dist, distParam);
		// First entry in the output is the desirability exponent term.
		// Second entry is the desirability value assigned to each destination.
		// Third entry is the list of all possible desirability values.
		double [][] desOut = desExp(person, entSched, destActs, desParam, expParam);
		// Desirability term.
		double [] desExp = desOut[0];
		// Matrix of Markov exponents.
		double [][] markovConsts = markovMatToExp(transMat);
		// Select the Markov exponents for the current destination.
		double [] markovExp = markovConsts[currDest];
		
		// For each destination...
		for (int w = 0; w < exponent.length; w++) {
			//...calculate the total cost function for each destination...
			exponent[w] = busyExp[w] + distExp[w] + desExp[w] + markovExp[w];
			//...and thence the unnormalised probabilities.
			probabilities[w] = Math.exp(exponent[w]);
		}
		// If the new next destination constraint is being enforced...
		if (nextDestConstr) {
			//...then the probability of the current destination is zero.
			probabilities[person.destination] = 0;
		}
		// Assign next destination.
		nextDest = nextDest(probabilities);
		// Assume that the next activity is the first one in the destination.
		person.currAct = destActs[nextDest][0];
		return nextDest;
	}

	/* 
	Determine the next destination for an agent using a multinomial logit model 
	with no Markov included.  This model includes occupancy of destinations, the 
	distance of the agent from each destination, and the desirability of the 
	destination based on the agent's schedule and the activities possible at each 
	destination.
	Requires the following inputs:
	1. The index of the agent making the choice.
	2. Value of the occupancy parameter.
	3. Value of the distance parameter.
	4. Value of the desirability parameter.
	5. Array of occupancies for all destinations.
	6. Matrix of available activities at each destination.  Assumes that each 
	destination has the same number of available activities.
	7. An array of matrices giving the distance of each spatial grid cell from 
	every destination.
	8. The spatial grid position of the agent.
	9. Is the new next destination constraint being enforced?
	10. The parameter of the negative exponential function used to calculate 
	desirabilities.
	*/
	public int fullModel (agent person, double busyParam, double distParam, 
	double desParam, int [] destOccs, char [][] destActs, double [][][] dist,
	int [] currPos, boolean nextDestConstr, double expParam) {
		// Initialise the next destination.
		int nextDest = 0;
		// Total number of destinations.
		int numDest = destOccs.length;
		ArrayList<Character> entSched = person.schedule;
		// Create array of expontial probabilities according to occupancy.
		double [] exponent = new double [numDest];
		// Create corresponding array of destination choice probabilities.
		double [] probabilities = new double [numDest];

		// Calculate each term of the cost function for each destination.
		// Occupancy term.
		double [] busyExp = scaledBusyExp(destOccs, busyParam);
		// Distance term.
		double [] distExp = scaledDistExp(person, currPos, dist, distParam);

		// First entry in the output is the desirability exponent term.
		// Second entry is the desirability value assigned to each destination.
		// Third entry is the list of all possible desirability values.
		double [][] desOut = scaledDesExp(person, entSched, destActs, desParam, expParam);
		// Desirability term.
		double [] desExp = desOut[0];

		// For each destination...
		for (int w = 0; w < exponent.length; w++) {
			//...calculate the total cost function for each destination...
			exponent[w] = busyExp[w] + distExp[w] + desExp[w];
			//...and thence the unnormalised probabilities.
			probabilities[w] = Math.exp(exponent[w]);
		}
		// If the new next destination constraint is being enforced...
		if (nextDestConstr) {
			//...then the probability of the current destination is zero.
			probabilities[person.destination] = 0;
		}

		// Assign next destination.
		nextDest = nextDest(probabilities);
		// Assume that the next activity is the first one in the destination.
		person.currAct = destActs[nextDest][0];
		return nextDest;
	}

	/*
	Select a destination from the full range of possible destinations based 
	on an array of probability values.
	Requires the following input:
	1. An array of probabilities with one value per destination available.
	*/
	public int nextDest (double [] probabilities) {
		// New random number generator.
		Random mouseTrap = new Random();
		// Initialise the next destination.
		int nextDest = 0;
		// Normalisation constant for the input array.
		double normConst = 0;
		// Cumulative sum array for destination selection.
		double [] cumSum = new double [probabilities.length];
		// Draw from standard uniform distribution.
		double selection = mouseTrap.nextDouble();
		// Calculate the normalisation constant.
		for (int q = 0; q < probabilities.length; q++) {
			normConst += probabilities[q];
		}

		// Normalise the probabilities.
		for (int e = 0; e < probabilities.length; e++) {
			probabilities[e] /= normConst;
		}

		// Start the cumulative sum off at the first probability.
		cumSum[0] = probabilities[0];
		// For each probability value...
		for (int u = 1; u < probabilities.length; u++) {
			//...calculate the running cumulative sum.
			cumSum[u] = cumSum[u - 1] + probabilities[u];
		}

		// For each probability value/alternative...
		for (int f = 0; f < probabilities.length; f++) {
			/*...find where it sits in the cumulative entries.  This determines 
			the next destination such that if the cumsum[dest] <= selection < 
			cumsum[DEST], then DEST is next destination.*/
			if (selection < cumSum[f]) {
				// Assign next destination.
				nextDest = f;
				break;
			}
		}
		return nextDest;
	}

	/*
	Select a destination that is not the current destination from all other 
	destinations based on an array of probability values.
	requires the following inputs:
	1. An array of probabilities with one value per destination available.
	2. The current destination of the agent.
	*/
	public int nextDest (double [] probabilities, int currDest) {
		// New random number generator.
		Random mouseTrap = new Random();
		// Initialise the next destination.
		int nextDest = 0;
		// Normalisation constant for the input array.
		double normConst = 0;
		// Cumulative sum array for destination selection.
		double [] cumSum = new double [probabilities.length];
		// Draw from standard uniform distribution.
		double selection = mouseTrap.nextDouble();
		// Calculate the normalisation constant.
		for (int q = 0; q < probabilities.length; q++) {
			normConst += probabilities[q];
		}
		// Normalise the probabilities.
		for (int e = 0; e < probabilities.length; e++) {
			probabilities[e] /= normConst;
		}

		// Start the cumulative sum off at the first probability.
		cumSum[0] = probabilities[0];
		// For each probability value...
		for (int u = 1; u < probabilities.length; u++) {
			//...calculate the running cumulative sum.
			cumSum[u] = cumSum[u - 1] + probabilities[u];
		}

		// For each probability value/alternative...
		for (int f = 0; f < probabilities.length; f++) {
			/*...find where it sits in the cumulative entries.  This determines 
			the next destination such that if the cumsum[dest] <= selection < 
			cumsum[DEST], then DEST is next destination.*/
			if (selection < cumSum[f]) {
				// Assign next destination.
				nextDest = f;
				break;
			}
		}
		return nextDest;
	}

	public void out(int i) { // method which can be used to write information out.
		System.out.println("people " + i + " x " + people[i].x + " y " + people[i].y);
	}

	/* Sample a value from an exponential distribution.  Used to generate waiting 
	times for agents which arrive at their chosen destination.
	Requires the folloing input:
	1. Value of the exponential distribution parameter.
	*/
	public int getNextIntExpDistSampling(double lambda) {
		// New random number generator for...
		Random buckaroo = new Random();
		//...sampling from a standard uniform distribution.
		double unifValue = buckaroo.nextDouble();
		// Convert standard unifrom to that of an exponential distribution.
		double expValue = -(1/lambda) * Math.log(1 - unifValue);
		return ((int) Math.round(expValue));
	}

	/* This method produces a normally distributed angle, mean = 0, sd = 1.
	Requires the following inputs:
	1. X-component of a vector.
	2. Y-component of a vector.
	Not used.
	*/
	public double nextGaussian(double x1, double x2) { 
		// see "Box-Muller transform" on wikipedia, polar method.
		x1 = 2 * x1 - 1.0;
		x2 = 2 * x2 - 1.0;
		double w = x1 * x1 + x2 * x2;

		Random boggle = new Random();

		while (w > 1) {
			x1 = 2 * boggle.nextDouble() - 1.0;
			x2 = 2 * boggle.nextDouble() - 1.0;
			w = x1 * x1 + x2 * x2;
		}
		
		return Math.sqrt(-2.0 * Math.log(w) / w) * x1;
	}

	/* Write the sequence of destinations CHOSEN by every agent over the course
	of a simulation, along with the corresponding sequence of decision times, 
	to a file.  Each agent has two lines of comma-separated values, the first 
	being the sequence of timesteps and the second being the destinations chosen.
	This version takes the name of the output file and of the output directory
	as input.*/
	public void writeDestSequence(String filename, String dir) {
		//System.out.println("Writing agent destination sequence to file.");
		// Try to open a connection to the file.
		try {
			FileWriter file = new FileWriter(dir + "/" + filename);
			pwout = new PrintWriter(file);
		}
		// If it fails, tell us why.
		catch (IOException e) {
			e.printStackTrace();
		}
		// For each agent...
		for (int h = 0; h < N; h++) {
			// Convert their destination and decision time sequences into arrays.
			Integer[] destinations = people[h].dests.toArray(new Integer[0]);
			Integer[] choiceTimes = people[h].decisionTimes.toArray(new Integer[0]);
			//System.out.println(Arrays.toString(destinations));
			//System.out.println(destinations.length);
			// Check whether the two sequences are the same length, and warn if not.
			if (choiceTimes.length != destinations.length) {
				System.out.println("WARNING! Choice time sequence and destination sequence for agent " + 
				h + " are not equal!");
				System.out.println("The choice time sequence has length = " + choiceTimes.length + 
				" and the destination sequence has length " + destinations.length + "!");
				System.out.println(people[h].dests.toString());
				System.out.println(people[h].decisionTimes.toString());
			}
			// If the agent made at least one choice...
			if (choiceTimes.length != 0) {
				// Write the comma-separated choice times on one line.
				for (int w = 0; w < choiceTimes.length; w++) {
					pwout.print(choiceTimes[w] + ",");
					//System.out.println(destinations[w]);
				}
				// Start a new line.
				pwout.println();
				// Write each destination chosen to line.
				for (int w = 0; w < destinations.length; w++) {
					pwout.print(destinations[w] + ",");
					//System.out.println(destinations[w]);
				}
				// Start a new line.
				pwout.println();
			}
		}
		pwout.close();
	}

	/*
	Write the sequence of destinations CHOSEN by every agent over the course
	of a simulation, along with the corresponding sequence of decision times, 
	to a file.  Each agent has two lines of comma-separated values, the first 
	being the sequence of timesteps and the second being the destinations chosen.
	Requires the following inputs:
	1. A printwriter object for the file.
	2. A value of the equilibration time.  Only choices after this time are recorded.
	*/
	public void writeDestSequence(PrintWriter name, int waitTime) {
		// Create lists to hold the choice times and destinations after the 
		// equilibration time.
		List<Integer> recTimes;
		List<Integer> recSeq;
		// Are choices being recorded?
		boolean recChoices;
		// For each agent...
		for (int h = 0; h < N; h++) {
			//...print the agent ID on one line.
			name.print(h);
			// Create a new line.
			name.println();
			// Assume that no choices will be recorded.
			recChoices = false;
			// Select the decision times and chosen destinations for this agent.
			recTimes = people[h].decisionTimes.subList(0, people[h].decisionTimes.size());
			recSeq = people[h].dests.subList(0, people[h].dests.size());
			// For each decision made by this agent...
			for (int p = 0; p < people[h].decisionTimes.size(); p++) {
				//...if the decision time is after the equilibration time...
				if (people[h].decisionTimes.get(p) >= waitTime) {
					//...then record the decision time and chosen destination.
					recTimes= people[h].decisionTimes.subList(p, people[h].decisionTimes.size());
					recSeq= people[h].dests.subList(p, people[h].dests.size());
					// There is now a record of at least one choice made by an agent in 
					// this simulation.
					recChoices = true;
					break;
				}
			}
			// If there are recorded choices...
			if (recChoices){
				//...convert their destination and decision time sequences into arrays.
				Integer[] destinations = recSeq.toArray(new Integer[0]);
				Integer[] choiceTimes = recTimes.toArray(new Integer[0]);

				// Check whether the two sequences are the same length, and warn if not.
				if (choiceTimes.length != destinations.length) {
					System.out.println("WARNING! Choice time sequence and destination sequence for agent " + 
					h + " are not equal!");
					System.out.println("The choice time sequence has length = " + choiceTimes.length + 
					" and the destination sequence has length " + destinations.length + "!");
					System.out.println(people[h].dests.toString());
					System.out.println(people[h].decisionTimes.toString());
				}

				// If the agent made at least one choice...
				if (choiceTimes.length != 0) {
					//...write the comma-separated choice times on one line.
					for (int w = 0; w < choiceTimes.length; w++) {
						name.print(choiceTimes[w] + ",");
					}
					// Start a new line.
					name.println();
					// Write each destination chosen to line.
					for (int w = 0; w < destinations.length; w++) {
						name.print(destinations[w] + ",");
					}
					// Start a new line.
					name.println();
				}
			}
		}
	}

	/*
	Write the distances of every agent from each destination at the time a choice
	was made into a file.  Creates a file of comma-separated values, separated by 
	agent with the first line being the agent ID, then each line is the distances
	from every destination at each decision, this repeats for all agents.
	Requires the following inputs:
	1. A printwriter object for the file.
	2. A value of the equilibration time.  Only distances of choices made after 
	this time are recorded.
	*/
	public void writeDistances(PrintWriter name, int waitTime) {
		// Create lists to hold the distances at each choice time.
		List<List<Double>> recDists;
		// Do we record the distances now or not?
		boolean distRec;
		// For each agent...
		for (int h = 0; h < N; h++) {
			//...lets assume that we aren't recording distances yet.
			distRec = false;
			//...print the agent ID on one line.
			name.print(h);
			// Start a new line.
			name.println();
			// Start a list which records the distance of the agent from all 
			// destinations.
			recDists = people[h].dists.subList(0, people[h].dists.size());
			// For each decision an agent made...
			for (int p = 0; p < people[h].decisionTimes.size(); p++) {
				//...if the decision was made after the equilibration time...
				if (people[h].decisionTimes.get(p) >= waitTime) {
					//...then write it down...
					recDists = people[h].dists.subList(p, people[h].dists.size());
					//...and make sure that you now record the distances from now on.
					distRec= true;
					break;
				}
			}
			// If we are recording distance now...
			if (distRec) {
				//...for every decision that agent made...
				for (int i = 0; i < recDists.size(); i++) {
					//...convert the distances from each destination at decision time into 
					// an array...
					Double[] distances = recDists.get(i).toArray(new Double[0]);
					//...so that each destination distance can be written easily to a line.
					for (int w = 0; w < distances.length; w++) {
						// Comma-separated values.
						name.print(distances[w] + ",");
					}
					// One line per decision.
					name.println();
				}
			}
		}
	}

	/*
	Write the desirabilities of every agent from each destination at the 
	time a choice was made into a file.  Creates a file of comma-separated 
	values, separated by agent with the first line being the agent ID, then 
	each line is the desirabilities	from every destination at each decision, 
	this repeats for all agents.
	Requires the following inputs:
	1. A printwriter object for the file.
	2. A value of the equilibration time.  Only distances of choices made after 
	this time are recorded.
	*/
	public void writeDesires(PrintWriter name, int waitTime) {
		// Create a list to hold the desirabilities of all destinations at each 
		// choice time.
		List<List<Double>> recDes;
		// Do we record the desirabilities now or not?
		boolean desRec;
		// For each agent...
		for (int h = 0; h < N; h++) {
			//...lets assume that we aren't recording desirabilities yet.
			desRec = false;
			// Print the agent ID on one line.
			name.print(h);
			// Start a new line.
			name.println();
			// Start a list which records the desirability of the agent for all 
			// destinations.
			recDes = people[h].desires.subList(0, people[h].desires.size());
			// For each decision an agent made...
			for (int p = 0; p < people[h].decisionTimes.size(); p++) {
				//...if the decision was made after the equilibration time...
				if (people[h].decisionTimes.get(p) >= waitTime) {
					//...then write it down...
					recDes = people[h].desires.subList(p, people[h].desires.size());
					//...and make sure that you now record the desirabilities from now on.
					desRec= true;
					break;
				}
			}
			// If we are recording desirability now...
			if (desRec) {
				//...for every decision that agent made...
				for (int i = 0; i < recDes.size(); i++) {
					//...convert the distances from each destination at decision 
					// time into an array...
					Double[] desires = recDes.get(i).toArray(new Double[0]);
					//...so that each destination distance can be written easily to a line.
					for (int w = 0; w < desires.length; w++) {
						// Comma-separated values.
						name.print(desires[w] + ",");
					}
					// One line per decision.
					name.println();
				}
			}
		}
	}

	/*
	Write the schedules of every agent after each choice into a file.  Creates 
	a file of comma-separated values, separated by agent with the first line being 
	the agent ID, then each line is the schedule of that agent after each decision 
	that they made, this repeats for all agents.
	Requires the following inputs:
	1. A printwriter object for the file.
	2. A value of the equilibration time.  Only distances of choices made after 
	this time are recorded.
	*/
	public void writeEntSchedHist(PrintWriter name, int waitTime) {
		// Create a list to hold the schedules of agents after each decision is made.
		List<Character[]> recScheds;
		// Do we record the agent schedules now or not?
		boolean schedRec;
		// For each agent...
		for (int h = 0; h < N; h++) {
			//...lets assume that we aren't recording schedules yet.
			schedRec = false;
			// Print the agent ID on one line.
			name.print(h);
			// Start a new line.
			name.println();
			// Extract the schedules after each decision made by the agent.
			recScheds = people[h].schedHist.subList(0, people[h].schedHist.size());

			// For each decision made by this agent...
			for (int p = 0; p < people[h].decisionTimes.size(); p++) {
				//...if the decision was made after the equilibration time...
				if (people[h].decisionTimes.get(p) >= waitTime) {
					//...then write it down...
					recScheds = people[h].schedHist.subList(p, people[h].schedHist.size());
					//...and make sure that you now record the desirabilities from now on.
					schedRec = true;
					break;
				}
			}
			// If we are recording schedules now...
			if (schedRec) {
				//...for every change in the agent's schedule...
				for (int i = 0; i < recScheds.size(); i++) {
					//...select the entry in the ArrayList.
					Character[] schedHistory = recScheds.get(i);

					// For each entry in that version of the schedule...
					for (int w = 0; w < schedHistory.length; w++) {
						//...write each of them on one line, separated by commas.
						name.print(schedHistory[w] + ",");
					}
					// Start a new line for the next schedule.
					name.println();
				}
			}
		}
	}
}