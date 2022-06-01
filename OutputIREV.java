
//import java.awt.*;
//import java.awt.event.*;
//import java.lang.Math;
//import java.lang.Number;
//import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;

/* 
Program to run the agent-based simulator without the need for a GUI or any further
user input once begun.  Faster than using GuiOutput in running simulations.  
Allows running of multiple simulation replicates at a time, but only with one set 
of simulation settings, e.g. one set of parameter values, one environment etc..

Run from the Windows Command Prompt 
"java OutputIREV filename (absolutefilepathtodirectorycontainingfilename)", 
where filename is the name of the file providing the simulator settings.  
The file must have an identical format to /parafiles/initial.txt.  

Can write output data at the end of each simulation replicate.

This is a crucial part of the agent-based simulator used in King et. al 2022 
https://doi.org/10.1080/23249935.2021.2017510.  More details about how the 
simulator was used can be found there.
*/
public class OutputIREV

{
	// For writing output to files later.
	PrintWriter pwout, pwout1, pwout2, pwout3, pwout4, pwout5, pwout6;
	FileWriter out, out1, out2, out3, out4, out5, out6;
	// Names of each possible output file.
	String filename1, filename2, filename3, filename4, filename5, filename6;
	// The absolute filepath to store output for all simulation replicates.
	String directory;
	// Initialise the filename for the file containing environment layout, 
	// distances, and floor field values for each destination.
	String room = "test.txt";
	// Indices for the timestep and simulation replicate, respectively.
	int step, sim;
	/*
	The total number of agents, the size of the environment in metres, the total 
	number of timesteps, and the number of equilibration timesteps.
	*/
	int N, sizeTemp, steps, equilTime;
	/*
	The dimensions of the spatial grid (length of one side of the square) in 
	centimetres, the number of destinations in the environment, and the number of 
	activities available.
	*/
	int gridSize, numDest, numAct;
	// Public version of the total number of agents needed as input to the constructor.
	public int Ntemp;
	// Is an environment being read from a file or is a default environment created? 
	// Need to change this if you do/don't want to use room specified in input.
	public boolean readroom = true;

	/*
	1. Can agent masses vary?
	2. Can agent radii vary?
	3. Can agent preferred speeds vary?
	4. Is agent trajectory output desired?
	5. Is destination occupancy output desired?
	6. Is chosen destination sequence output desired?
	*/
	boolean varyMass, varySize, varySpeed, trajOutput, occOutput, destSeqOutput;
	/*
	1. Is destination distance output desired?
	2. Is destination desirability output desired?
	3. Is agent schedule output desired?
	4. Is a Markov model included in the destination choice model?
	5. Is the new next destination constraint being enforced?
	*/
	boolean destDistOutput, desOutput, entSchedOutput, markov, newDest;

	// Initialise floor field and distance values for the spatial grid for each 
	// destination.
	public double[][][] fstatictemp;
	public double[][][] disttemp;
	// Note the name of the room to be used, if any.
	public String type;
	// The x- and y-coordinates of the destinations.
	public int[] extmp;
	public int[] eytmp;

	// The initial positions, masses, radii, and preferred speeds of agents, if 
	// initial agent positions and properties are provided.  Another double array 
	// giving values for all destination choice model parameters.
	public double[] xIn, yIn, massIn, sizeIn, speedIn, params;
	// Object for the group of agents.
	IREV1 crowd;

	// Basically a list of all possible activities (represented as letters).
	char [] alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray();
	// 2D array of the possible activities at each destination.
	public char[][] possDestActs;

	// Random number generator used to assign the number of possible activities at 
	// each destination.  Not currently used.
	Random gameOfLife = new Random();

	/*
	* Constructor method. Takes the following inputs: 
	* 1. Number of agents. 
	* 2. Number of timesteps. 
	* 3. Number of activities available in the simulation.
	* 4. Number of simulations to run.
	* 5. The name of the room file to be used. 
	* 6. Whether or not masses of agents will be varied. 
	* 7. Whether or not the size of the agents will be varied. 
	* 8. Whether or not the preferred speed of the agents will be varied.  
	* 9. Whether or not to produce agent trajectory output. 
	* 10. Whether or not to produce destination occupancy output. 
	* 11. Whether or not to produce agent destination sequences. 
	* 12. Whether or not to produce agent destination distances. 
	* 13. Whether or not to produce agent agent schedules. 
	* 14. The initial x-coordinates of all agents. 
	* 15. The initial y-coordinates of all agents. 
	* 16. The initial masses of the agents. 
	* 17. The initial sizes of the agents. 
	* 18. The initial preferred speeds of the agents. 
	* 19. The name of the directory to send output files to. 
	* 20. The values of the model paramaters. First entry = busyness, 
	* 	   second = distance, third = desirability.
	* 21. Whether or not the Markov model is included.
	*/
	public OutputIREV(int Ntemp, int steps, int equilT, int numAct, int sims, 
		String room, boolean varyMass, boolean varySize, boolean varySpeed, 
		boolean trajOutput, boolean occOutput, boolean destSeqOutput, 
		boolean destDistOutput, boolean desOutput, boolean entSchedOutput,	
		double[] xIn, double[] yIn, double[] massIn, double[] sizeIn, 
		double[] speedIn, String directory, double [] params, boolean markov, 
		boolean newNextDest) {
		
		// Set the initial simulator conditions.
		this.Ntemp = Ntemp;
		this.room = room;
		this.varyMass = varyMass;
		this.varySize = varySize;
		this.varySpeed = varySpeed;
		this.trajOutput = trajOutput;
		this.occOutput = occOutput;
		this.destSeqOutput = destSeqOutput;
		this.destDistOutput = destDistOutput;
		this.desOutput = desOutput;
		this.entSchedOutput = entSchedOutput;
		this.xIn = xIn;
		this.yIn = yIn;
		this.massIn = massIn;
		this.sizeIn = sizeIn;
		this.speedIn = speedIn;
		this.directory = directory;
		this.steps = steps;
		this.equilTime = equilT;
		this.sim = sims;
		N = Ntemp;
		this.numAct = numAct;
		this.params = params;
		this.markov = markov;
		this.newDest = newNextDest;

		// Initialise the timstep index.
		step = 0;
		// Begin the simulation.
		actionPerformed();
	}

	// Function which runs a simulation.
	public void actionPerformed() {
		// Currently set up to use the model specified in King et al. 2022
		// https://doi.org/10.1080/23249935.2021.2017510.
		double occParam = params[0]; // busyness model parameter
		double distParam = params[1]; // distance model parameter
		double desParam = params[2]; // desirability model parameter

		/* Now create a character array of a certain length of the first numAct
		letters in the alphabet array which represent the possible activities 
		that can be performed in this simulation.*/
		char [] allActsArr = Arrays.copyOfRange(alphabet, 0, numAct);

		/* Now create a mutable ArrayList copy of the array of the possible 
		activities for use in IREV1 call later.*/
		ArrayList<Character> allActs = new ArrayList<Character>();
		for (int i = 0; i < allActsArr.length; i++) {
			allActs.add(allActsArr[i]);
		}

		// If we want to read in a spatial configuration from a file...
		if (readroom) {
			try {
				// Create streams so that the room file can be read, if it is where
				// you say it is.
				FileInputStream fstream = new FileInputStream("C:/Temp/Simulator/Rooms/" + room); // single file
				//FileInputStream fstream = new FileInputStream("../Rooms/" + room); // multi-file
				// Get the object of DataInputStream
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));

				// Read in which room you're using from the room file.
				type = br.readLine();
				// Read in the size of the room from the room file.
				gridSize = Integer.valueOf(br.readLine()).intValue();
				// Read in the number of destinations from the room file.
				numDest = Integer.valueOf(br.readLine()).intValue();
				// Initialise the floor fields and distances for every destination.
				fstatictemp = new double[numDest][gridSize][gridSize];
				disttemp = new double[numDest][gridSize][gridSize];
				// Initialise the destination coordinates.
				extmp = new int[numDest];
				eytmp = new int[numDest];
				// Read in the coordinates of the destinations.
				for (int u = 0; u < numDest; u++) {
					String line = br.readLine();
					// Separates contents of line around the ','.
					String[] parts = line.split(",");
					// x-coordinates.
					extmp[u] = Integer.valueOf(parts[0]).intValue();
					// y-coordinates
					eytmp[u] = Integer.valueOf(parts[1]).intValue();
				}

				// Read in the floor field:
				int count1 = 0; // line/row counter
				int count2 = 0; // element/column counter
				String line = br.readLine();
				// Separates contents lines around the ','.
				String[] parts;
				int fieldIndex = 0; // what field we are on
				// Over the entire number of floor fields expected...
				while (fieldIndex < numDest) {					
					//...assign floor field values to each grid cell.
					while (count1 <= (gridSize - 1)) {
						parts = line.split(",");
						// Assign floor field values now.
						fstatictemp[fieldIndex][count1][count2] = Double.valueOf(parts[count2]).doubleValue();
						count2 += 1; // increment column index
						// If we reach the end of a row...
						if (count2 == (gridSize)) {
							//...read in the next line...
							line = br.readLine();
							//...reset the column index...
							count2 = 0;
							//...and increment the row index.
							count1 += 1;
						}
					}
					// Assign distances from each destination to each grid cell.
					count1 = 0; // line/row counter
					count2 = 0; // element/column counter
					while (count1 <= (gridSize - 1)) {
						parts = line.split(",");
						// Assign distance values now.
						disttemp[fieldIndex][count1][count2] = Double.valueOf(parts[count2]).doubleValue();
						count2 += 1; // increment column index
						// If we reach the end of a row...
						if (count2 == (gridSize)) {
							//...read in the next line...
							line = br.readLine();
							//...reset the column index...
							count2 = 0;
							//...and increment the row index.
							count1 += 1;
						}
					}
					// Increment field index without it going out of bounds.
					fieldIndex++;
					// Reset row and column counters for this iteration of the loop.
					count1 = 0; // line/row counter
					count2 = 0; // element/column counter
				}
				// Need to assign the right value for the environment size:
				sizeTemp = gridSize;
				// Close the connection to the room file.
				br.close();
				
				/*
				Now let's assign activities to each destination.  
				This can be a fixed configuration or randomly assigned.  
				Currently fixed so that each destination has one unique activity.
				*/
				possDestActs = new char[numDest][];
				// For each destination...
				for (int i = 0; i < possDestActs.length; i++) {
					// Random assignment of activities.
					/*char [] destAct = new char[gameOfLife.nextInt(allActsArr.length) + 1];
					//System.out.println(destAct.length);
					Collections.shuffle(allActs);
					for (int y = 0; y < destAct.length; y++) {
						destAct[y] = allActs.get(y);
					}*/

					// Specific destination activity configuration.
					// This one is each destination has one activity...
					char [] destAct = new char[1];
					//...which is the corresponding entry in the activity ArrayList.
					destAct[0] = allActs.get(i);
					// Collate all activities for this destination into structure holding 
					// activities for all destinations.
					possDestActs[i] = destAct;
				} 
			} catch (IOException e) {
				// If reading from the file fails, tell us why.
				e.printStackTrace();
			}
		} else { // Otherwise, if we are not reading an environment from a file...
			// Default destination coordinates if none specificed.
			int exitxtmp = -10;
			int exitytmp = -10;
			// Default parameter value:
			sizeTemp = 100;
			type = "noexit"; // empty box room
			extmp = new int[1]; // default 1 destination.
			eytmp = new int[1]; // default 1 destination.
			for (int yy = 0; yy < extmp.length; yy++) {
				extmp[yy] = exitxtmp;
				eytmp[yy] = exitytmp;
			}
		}

		// Declare the main simulation.

		// If the Markov model is to be included...
		if (markov) {
			//...initialise the transition matrix with the dimensions = number of
			// destinations.
			double[][] transMat = new double[extmp.length][extmp.length];
			try {
				/* Create streams to read in the file containing the entries of 
				the transition matrix, if it is where you say it is. */
				FileInputStream fstream = new FileInputStream(directory + "/trans_mat.txt");
				// Get the object of DataInputStream.
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));
				// For each row of the matrix...
				for (int i = 0; i < extmp.length; i++) {
					String line = br.readLine();
					// Separates contents of line around the ','.
					String[] parts = line.split(",");
					// For each value in the row...
					for (int j = 0; j < parts.length; j++) {
						//...set the corresponding element in the transition matrix.
						transMat[i][j] = Double.valueOf(parts[j]);
					}
				}
				// Close the connection to the transition matrix file.
				br.close();
			} catch (IOException e) {
				/* If there is currently no file for the transition matrix, 
				make one and then use that matrix instead.*/

				// Initialise the normalisation constant to 0.
				double normConstant = 0;
				// New random number generator for choosing matrix elements.
				Random twistor = new Random();
				// Assign the filename for the transition matrix file.
				String filename = directory + "/trans_mat.txt";
				// Create output file in stated directory.
				try {
					out = new FileWriter(filename);
					// Create a writer object ready to write to said file.
					pwout = new PrintWriter(out);
				} catch (IOException err) {
					// If that fails, tell us why.
					err.printStackTrace();
				}
				// For each row...
				for (int r = 0; r < extmp.length; r++) {
					//...start normalisation constant at 0.
					normConstant = 0;
					// For each value of the row...
					for (int c = 0; c < extmp.length; c++) {
						// Matrix must be symmetric.
						if (c >= r) {
							transMat[r][c] = twistor.nextDouble();
							transMat[c][r] = transMat[r][c];
						}
						// Increment normalisation constant for the row.
						normConstant += transMat[r][c];
					}
					// Now divide each value by said constant.
					for (int c = 0; c < extmp.length; c++) {
						transMat[r][c] /= normConstant;
						// Write normalised element to file.
						pwout.print(transMat[r][c] + ",");
					}
					pwout.println();
				}
				// Now try to close the connection.
				try {
					out.close();
					pwout.close();
				} catch (IOException er) {
					// If it fails, tell us why.
					er.printStackTrace();
				}
			}

			/* Create the crowd of N agents in a spatial grid of dimension sizeTemp, 
			with destinations with coordinates (extmp, eytmp), params destination
			choice model parameters, the specified transition matrix, the 
			possible activities that can be performed, and the activities that 
			can be performed at each destination.*/
			crowd = new IREV1(N, sizeTemp, extmp, eytmp, params, transMat, allActs, possDestActs, newDest);
		} else { // Otherwise, if the Markov model is not included...
			/* Create the crowd of N agents in a spatial grid of dimension sizeTemp, 
			with destinations with coordinates (extmp, eytmp), params destination
			choice model parameters, the possible activities that can be performed,
			and the activities that can be performed at each destination.*/
			crowd = new IREV1(N, sizeTemp, extmp, eytmp, params, allActs, possDestActs, newDest);
		}

		/* If you're reading in a specific room/spatial configuration, assign 
		the correct static floor field and grid cell distances to the crowd object...*/
		if (readroom) {
			//...for each destination...
			for (int mm = 0; mm < extmp.length; mm++) {
				//...for each row in the spatial grid...
				for (int ll = 0; ll < crowd.gridSize; ll++) {
					//...for each value in the row...
					for (int kk = 0; kk < crowd.gridSize; kk++) {
						//...set the values of the floor field and distance from the given
						// destination for this cell.
						crowd.fstatic[mm][ll][kk] = fstatictemp[mm][ll][kk];
						crowd.distances[mm][ll][kk] = disttemp[mm][ll][kk];
					}
				}
			}
		} 
		// Otherwise, set the floor field as 0 for obstacles and 1 otherwise.
		else {
			// For each destination...
			for (int i = 1; i < extmp.length; i++) {
				//...for each row of the spatial grid...
				for (int ll = 0; ll < crowd.gridSize; ll++) {
					//...for each value in the row...
					for (int kk = 0; kk < crowd.gridSize; kk++) {
						//...if the floor field of the "1st" destination is 0...
						if (crowd.fstatic[0][ll][kk] == 0) {
							//...then the other floor fields are 0 too.
							crowd.fstatic[i][ll][kk] = 0;
						} else {
							// Otherwise, they are 1.
							crowd.fstatic[i][ll][kk] = 1;
						}
						// Since distance unimportant in default, set to 0.
						crowd.distances[i][ll][kk] = 0;
					}
				}
			}
		}

		// If agents can have variable preferred speeds...
		if (varySpeed) {
			//...then give the agents varied preferred speeds.
			crowd.varySpeed = true;
		}
		// If agents can have variable radii...
		if (varySize) {
			//...then give the agents varied radii.
			crowd.varySize = true;
		}
		// If agents can have variable masses...
		if (varyMass) {
			//...then give the agents varied masses.
			crowd.varyMass = true;
		}

		/*
		 * Assign starting positions and agent characteristics, if this is
		 * required.
		 */

		if (xIn[0] != 0.0) {
			// For each agent...
			for (int u = 0; u < N; u++) {
				//...set its initial position...
				crowd.people[u].x = xIn[u];
				crowd.people[u].y = yIn[u];
				//...its mass...
				crowd.people[u].mass = massIn[u];
				//...radius...
				crowd.people[u].size = sizeIn[u];
				//...and preferred speed.
				crowd.people[u].speed = speedIn[u];
				// Attempt to place agent at specified position.
				crowd.setInitialPosgiven();
			}
		} 
		// Otherwise, if initial agent positions are not provided...
		else {
			// Assign initial positions of agents according to floor field/layout:
			crowd.setInitial(0);
		}

		/* Initialise the output files with a name and the first line of 
		simulation information.*/
		// If agent trajectories are desired...
		if (trajOutput) {
			//...state the filename...
			filename1 = "busy=" + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_ent_trajs_" 
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
				//...and try to open a connection to it.
			try {
				out1 = new FileWriter(directory + "/" + filename1);
				pwout1 = new PrintWriter(out1);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout1.println("type = " + type + ", gridSize = " + crowd.gridSize + 
				", steps = " + steps + ", Number of destinations = " + 
				numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
		}

		// If destination occupancies are desired...
		if (occOutput) {
			//...state the filename...
			filename2 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_occ_"
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
			//...and try to open a connection to it.
			try {
				out2 = new FileWriter(directory + "/" + filename2);
				pwout2 = new PrintWriter(out2);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout2.println("type = " + type + ", gridSize = " + crowd.gridSize + 
				", steps = " + steps + ", Number of destinations = " + 
				numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
		}

		// If entity destination sequences are desired...
		if (destSeqOutput) {
			//...state the filename...
			filename3 = "busy=" + occParam + "_dist=" +
				distParam + "_des=" + desParam + "_dest_seq_" 
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
			//...and try to open a connection to it.
			try {
				out3 = new FileWriter(directory + "/" + filename3);
				pwout3 = new PrintWriter(out3);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout3.println("type = " + type + ", gridSize = " + crowd.gridSize + 
				", steps = " + steps + ", Number of destinations = " + 
				numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
			// File also contains the transition matrix used, if Markov included...
			if (markov) {
				//...for each row of the transition matrix...
				for (int i = 0; i < crowd.transMat.length; i++) {
					//...for each element of the row...
					for (int j = 0; j < crowd.transMat[i].length; j++) {
						//...write it to the file.
						pwout3.print(crowd.transMat[i][j] + ",");
					}
					pwout3.println();
				}
			}
		}

		// If distances to each destination are desired...
		if (destDistOutput) {
			//...state the filename...
			filename4 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_dist_"
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
			//...and try to open a connection to it.
			try {
				out4 = new FileWriter(directory + "/" + filename4);
				pwout4 = new PrintWriter(out4);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout4.println("type = " + type + ", gridSize = " + crowd.gridSize 
				+ ", steps = " + steps + ", Number of destinations = " 
				+ numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
		}

		// If destination desirabilities are desired...
		if (desOutput) {
			//...state the filename...
			filename5 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_des_"
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
			//...and try to open a connection to it.
			try {
				out5 = new FileWriter(directory + "/" + filename5);
				pwout5 = new PrintWriter(out5);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout5.println("type = " + type + ", gridSize = " + crowd.gridSize + 
				", steps = " + steps + ", Number of destinations = " + 
				numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
		}

		// If agent schedules are desired...
		if (entSchedOutput) {
			//...state the filename...
			filename6 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_ent_sched_"
				+ N + "_ents_sim_" + (sim + 1) + ".txt";
			//...and try to open a connection to it.
			try {
				out6 = new FileWriter(directory + "/" + filename6);
				pwout6 = new PrintWriter(out6);
			} catch (IOException e) {
				// If the connection fails, state why.
				e.printStackTrace();
			}
			/* First line of the file gives simulation information. Gives the 
			name of the space used, the size of the spatial grid, the number of 
			timesteps, the number of destinations, the number of agents, and the
			values of each paramater.*/
			pwout6.println("type = " + type + ", gridSize = " + crowd.gridSize + 
				", steps = " + steps + ", Number of destinations = " + 
				numDest + ", N = " + crowd.N + ", occupancy parameter = " 
				+ occParam + ", distance parameter = " + distParam + 
				", desirability parameter = " + desParam);
		}

//////////////////////////////////////////////// here perform simulation:

		/*
		* Now run each timestep/loop of the simulation. While we've 
		* performed less than the desired number of timesteps.
		*/
		while (step < steps) {
			step += 1;

			// Update the movement of every agent.
			crowd.updateMovement(step);
			// Update the choices of the agents, where necessary.
			crowd.updateChoices(step);

			// If we want agent trajectory output (equilibration time not yet used)...
			if (trajOutput) {
				//...for every agent...
				for (int u = 0; u < N; u++) {
					//...record the x positions of the agent.
					pwout1.print(crowd.people[u].x + ",");
				}
				// Start a new line.
				pwout1.println();
				// Now print the corresponding y-positions of every agent.
				// For every agent...
				for (int u = 0; u < N; u++) {
					//...record the y positions of the agent.
					pwout1.print(crowd.people[u].y + ",");
				}
				// Start a new line.
				pwout1.println();
			}
			// If we want destination occupancy output...
			if (occOutput) {
				//...if an equilibration time is being used...
				if (step >= equilTime){
					//...for each destination...
					for (int u = 0; u < crowd.destPopularity.length; u++) {
						//...record the occupancy at this timestep.
						pwout2.print(crowd.destPopularity[u] + ",");
					}
					// Start a new line.
					pwout2.println();
				}
			}
		}

		// If we want destination sequence output...
		if (destSeqOutput) {
			//...write agent's decision times and destination sequences to file.
			crowd.writeDestSequence(pwout3, equilTime);
			try {
				// Try to close the connection to the file.
				out3.close();
				pwout3.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
		// If we want destination distance output...
		if (destDistOutput) {
			//...write the destination distances to file.
			crowd.writeDistances(pwout4, equilTime);
			try {
				// Try to close the connection to the file.
				out4.close();
				pwout4.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
		// If we want destination desirability output...
		if (desOutput) {
			//...write destination desirabilities to file.
			crowd.writeDesires(pwout5, equilTime);
			try {
				// Try to close the connection to the file.
				out5.close();
				pwout5.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
		// If we want agent schedule output...
		if (entSchedOutput) {
			//...write agent's schedule history to file.
			crowd.writeEntSchedHist(pwout6, equilTime);
			try {
				// Try to close the connection to the file.
				out6.close();
				pwout6.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
		// If we have agent trajectory output...
		if (trajOutput) {
			try {
				// Try to close the connection to the file.
				out1.close();
				pwout1.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
		// If we have destination occupancy output...
		if (occOutput) {
			try {
				// Try to close the connection to the file.
				out2.close();
				pwout2.close();
			} catch (IOException e) {
				// If closing the connection fails, tell us why.
				e.printStackTrace();
			}
		}
	}

/* Main instance - describes code that is immediately run when the file is run
using the command "java OutputIREV inputfilename.txt"*/

	public static void main(String[] args) {

		// Initialise parameters.

		/* Number of agents, number of timesteps, starting simulation index 
		(usually 1, unless you want/need to start partway through a batch), 
		ending simulation index (total number of simulations = end - start), 
		total number of activities available in the scenario, and number of 
		equilibration timesteps.*/
		int Nin, steps, startSim, endSim, numActs, equilSteps; 
		/* Name of the simulator initial conditions file, name of the simulated 
		space/room file, absolute filepath to the directory to place any simulator 
		output, absolute filepath to directory where to find simulator initial 
		conditions file. */
		String inFilename, roomtmp, outDir, inDir;
		/* Indicators for whether: Markov model included, whether to vary mass,
		size, and speed of agents, and trajectory data outputted.*/
		boolean markov, varyMass1, varySize1, varySpeed1, trajOutput;
		/* Indicators for whether: destination occupancies outputted, destination
		sequences outputted, destination distances outputted, destination 
		desirabilities outputted.*/
		boolean occOutput, destSeqOutput, destDistOutput, desOutput; 
		// Indicator for whether agent schedule histories outputted.
		boolean entSchedOutput;
		// Indicator for whether the new next destination constraint is enforced.
		boolean newDest;
		// Placeholder for the boolean values in the input file.
		int tmptmp;
		// Are we reading in model parameters or setting them to default?
		boolean readParas = true;
		// Destination choice model parameters.  Currently using model described 
		// in King et al. 2022 https://doi.org/10.1080/23249935.2021.2017510.
		double occParam, distParam, desParam;
		/* Initial positions, masses, sizes, and speeds of agents, and the 
		array of model parameters.*/
		double[] xIn, yIn, massIn, sizeIn, speedIn, params;
		// String array to hold all the split strings from input file handling.
		String[] pieces;
		// State the level of accuracy to record destination choice model parameter 
		// values in the filenames.
		DecimalFormat df = new DecimalFormat("#.####");
		df.setRoundingMode(RoundingMode.CEILING);
		// Read in the name of the input file, if there is one.
		try {
			inFilename = args[0];
			// If the directory in which to find said input file is specified...
			if (args.length == 2) {
				//...then read it in from the terminal.
				inDir = args[1];
			}
			// Otherwise, if a directory is not specified...
			else {
				//...then it is assumed the file is in the "parafiles" directory.
				inDir = "Sim/parafiles"; // CHANGE THIS LINE FOR YOUR DIRECTORY STRUCTURE!
			}

			// If we want to read in parameters from a file...
			if (readParas) {
				//...set up the relevant connections, streams etc..
				FileInputStream fstream = new FileInputStream(inDir + "/" + inFilename); // single file
				//FileInputStream fstream = new FileInputStream(inFilename); // multi-file
				// Get the object of DataInputStream.
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));
				// Initialise array for holding model parameters.
				params = new double[3];

				// First line gives the starting simulation index.
				startSim = Integer.valueOf(br.readLine()).intValue();
				/* Second line gives the ending simulation index.  
				Total number of simulations = end - start.*/
				endSim = Integer.valueOf(br.readLine()).intValue();
				// Next line gives number of agents present.
				Nin = Integer.valueOf(br.readLine()).intValue();
				// Next line gives number of timesteps to run for.
				steps = Integer.valueOf(br.readLine()).intValue();
				// Next line give number of equilibration timesteps, after which 
				// simulation outputs are recorded.
				equilSteps = Integer.valueOf(br.readLine()).intValue();
				// Next line gives number of activity types available.
				numActs = Integer.valueOf(br.readLine()).intValue();
				// Next line gives the name of the simulated space.
				roomtmp = br.readLine();
				/* Extract the room name from room filename, assuming that room
				file name is room_name.txt.*/
				pieces = roomtmp.split("_");

				/*
				 Next lines describe the values of the parameters for the full model.
				*/
				String[] parameters = br.readLine().split(",");
				double par1 = (double) Double.valueOf(parameters[0]);
				double par2 = (double) Double.valueOf(parameters[1]);
				double par3 = (double) Double.valueOf(parameters[2]);
				params[0] = Double.valueOf(df.format(par1).toString());
				params[1] = Double.valueOf(df.format(par2).toString());
				params[2] = Double.valueOf(df.format(par3).toString());

				// tmptmp for Markov boolean.
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then Markov is included.
					markov = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then no Markov is included.
					markov = false;
				}

				// tmptmp for new destination constraint boolean.
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then the new next destination constraint is enforced.
					newDest = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then the new next destination constraint is not enforced.
					newDest = false;
				}

				/*
				* Next nine lines of file is a number deciding whether mass, size and speed of
				* agents should be varied and whether agent trajectories, destination
				* occupancies, chosen destination sequences, destination distances,
				* destination desirabilities, and/or entity schedule histories are required.
				*/
				// Vary masses?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then vary the masses of agents.
					varyMass1 = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then don't vary the masses of agents.
					varyMass1 = false;
				}
				// Vary agent radii?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then vary the radii of agents.
					varySize1 = true;
				}
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then don't vary the radii of agents.
					varySize1 = false;
				}
				// Vary preferred speeds?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then vary the preferred speeds of agents.
					varySpeed1 = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then don't vary the preferred speeds of agents.
					varySpeed1 = false;
				}
				// Agent trajectories output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then entity trajectories are output.
					trajOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then entity trajectories are not output.
					trajOutput = false;
				}
				// Destination occupancies output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then destination occupancies are output.
					occOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then destination occupancies are not output.
					occOutput = false;
				}
				// Agent destination sequences output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then chosen destination sequences are output.
					destSeqOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then chosen destination sequences are not output.
					destSeqOutput = false;
				}

				// Destination distance output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then destination distances are output.
					destDistOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then destination distances are not output.
					destDistOutput = false;
				}

				// Destination desirability output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then destination desirabilities are output.
					desOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then destination desirabilities are not output.
					desOutput = false;
				}
				// Agent schedules output?
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				// If tmptmp = 1...
				if (tmptmp == 1) {
					//...then agent schedules are output.
					entSchedOutput = true;
				} 
				// Otherwise, if tmptmp does not = 1...
				else {
					//...then agent schedules are not output.
					entSchedOutput = false;
				}

				// tmptmp for initial agent properties.
				tmptmp = Integer.valueOf(br.readLine()).intValue();
				xIn = new double[Nin]; // Initial x-positions of each agent
				yIn = new double[Nin]; // Initial y-position of each agent
				massIn = new double[Nin]; // Masses of each agent
				sizeIn = new double[Nin]; // Sizes of each agent
				speedIn = new double[Nin]; // Preferred speeds of each agent
				// If the value isn't 0...
				if (tmptmp != 0) {
					// ...then read in:
					// Initial x-positions.
					for (int u = 0; u < Nin; u++) {
						xIn[u] = Double.valueOf(br.readLine()).doubleValue();
					}
					// Initial y-positions.
					for (int u = 0; u < Nin; u++) {
						yIn[u] = Double.valueOf(br.readLine()).doubleValue();
					}
					// Masses.
					for (int u = 0; u < Nin; u++) {
						massIn[u] = Double.valueOf(br.readLine()).doubleValue();
					}
					// Sizes.
					for (int u = 0; u < Nin; u++) {
						sizeIn[u] = Double.valueOf(br.readLine()).doubleValue();
					}
					// Preferred speeds.
					for (int u = 0; u < Nin; u++) {
						speedIn[u] = Double.valueOf(br.readLine()).doubleValue();
					}
				} 
				// Otherwise, if tmptmp does not equal 1...
				else {
					//...initialise variables, just in case.
					for (int u = 0; u < Nin; u++) {
						xIn[u] = 0.0;
						yIn[u] = 0.0;
						massIn[u] = 0.0;
						sizeIn[u] = 0.0;
						speedIn[u] = 0.0;
					}
				}
				
				// Last line of the settings file should be the absolute filepath to 
				// place all outputs for all simulation replicates.
				outDir = br.readLine();
				// Close the connection to the settings file.
				br.close();
				in.close();
			} 
			// Otherwise, if a settings file is not provided...
			else {
				//...set default simulation setting values:
				/* These things need to be initialised in the else statement for the 
				program to compile. */
				// Only one simulation replicate.
				startSim = 1;
				endSim = 1;
				// Number of timesteps.
				steps = 10000;
				// Number of equilibration timesteps.
				equilSteps = 0;
				// Number of agents.
				Nin = 10;
				// Name of the room file to be used.
				roomtmp = "test.txt";
				// No Markov model included in destination choice.
				markov = false;
				// Agent masses constant.
				varyMass1 = false;
				// Agent radii constant.
				varySize1 = false;
				// Agent preferred speeds constant.
				varySpeed1 = false;
				// No agent trajectory output.
				trajOutput = false;
				// No destination occupancy output.
				occOutput = false;
				// No chosen destination sequence output.
				destSeqOutput = false;
				// No destination distance output.
				destDistOutput = false;
				// No destination desirability output.
				desOutput = false;
				// No agent schedule history output.
				entSchedOutput = false;
				// New next destination constraint not enforced.
				newDest = false;
				// Parameter values for model used in King et al. 2022
				// https://doi.org/10.1080/23249935.2021.2017510.
				params = new double[] {0, 0, 0};
				// Default environment name.
				pieces = new String[1];
				pieces[0] = "default";
				// One activity available.
				numActs = 1;
				// No initial agent properties.
				xIn = new double[Nin];
				yIn = new double[Nin];
				massIn = new double[Nin];
				sizeIn = new double[Nin];
				speedIn = new double[Nin];
				for (int u = 0; u < Nin; u++) {
					// All start at (0, 0) with 0 mass, size, pref speeds etc.
					xIn[u] = 0.0;
					yIn[u] = 0.0;
					massIn[u] = 0.0;
					sizeIn[u] = 0.0;
					speedIn[u] = 0.0;
				}
				// Default output directory here.
				outDir = "Sim"; // CHANGE THIS LINE FOR YOUR DIRECTORY STRUCTURE!
			}

			//////////////////////////// Now run all of the simulations

			// For each simulation...
			for (int s = startSim-1; s < endSim; s++) {
				//...print simulation index.
				System.out.println(s);
				// Specify the model parameters.
				occParam = params[0];
				distParam = params[1];
				desParam = params[2];
				// Directory must be hard-coded in, as output directory ends up
				// in parafiles when using the batch script.
				String directory = outDir
				+ "/" + pieces[0] + "_occ=" + occParam + "_dist="
				+ distParam + "_des=" + desParam;
				//System.out.println(directory);
				// Create the directory if it doesn't already exist.
				new File(directory).mkdirs();
				/* Now run the meat of the program.  Inputs are: number of 
				agents, number of timesteps, number of equilibration timesteps, 
				number of activities in the scenario, the current simulation index, the 
				name of the space, whether or not to vary agent masses, sizes, speeds, 
				whether or not to produce agent trajectory, destination occupancy, 
				agent destination sequence, agent destination distances, or 
				agent schedule outputs, the initial positions, masses, sizes, 
				speeds, of agents (if any), the directory to place output 
				files, the model parameters, whether the Markov model is 
				included, and whether the new next destination constraint is enforced.*/
				new OutputIREV(Nin, steps, equilSteps, numActs, s, roomtmp, 
				varyMass1, varySize1, varySpeed1, trajOutput, occOutput, 
				destSeqOutput, destDistOutput, desOutput, entSchedOutput, xIn, yIn, 
				massIn, sizeIn, speedIn, directory, params, markov, newDest);
			}
		} catch (IOException e) {
			// If no file provided, just display the error trace.
			e.printStackTrace();
		}
	}
}
