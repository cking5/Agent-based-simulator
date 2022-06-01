import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
//import java.lang.Math.*;
import java.lang.Number;
import java.awt.image.*;
import java.io.*;
import javax.imageio.*;
import java.util.*;

/*
Handles the running of the simulations, from setting the initial conditions to 
outputting data.  Allows the simulations to be visualised.

This is a crucial part of the agent-based simulator used in King et. al 2022 
https://doi.org/10.1080/23249935.2021.2017510.  More details about how the 
simulator was used can be found there.

GUI class for handling user interface.  Slower than running it through a terminal, 
but allows simulations to be visualised, with options for saving images at 
specific timesteps.  These images can then be used to create videos.  Can only 
generate data for one simulation at a time.  Can read in room files containing 
the environment layout, the distances of each point on the spatial grid from each 
destination, and the floor fields of all destinations.

Includes user input for:
	the size of the simulation window
	number of loops/timesteps to run the simulation for
	the number of agents present in the simulation
	a filename for the spatial and floor field configuration of a room
	the relative filepath from the location of this file to save output data

Also contains an option drop-down menu where you can choose whether:
	view the simulation as it runs (Output monitor)
	to use a room configuration from a file or not
	write output to a file
	the output should be detailed (include positions of every agent at every
	timestep).  Requires that output is written to a file. 
	output pictures of the simulation at every timestep
	read in the initial positions of the agents from a file
	the preferred speed of an agent is fixed or drawn from a normal distribution
	the mass of an agent should be fixed or drawn from a normal distribution
	the size/radii of each agent should be fixed or drawn from a normal distribution

Contains two buttons:
	1. Start - Begin a new simulation with the parameters and options selected.  
	   Simulation stops after the specified number of loops
	2. Cont - Continue the current simulation, running for another user-specified 
	   number of loops
*/
public class GuiOutput implements ActionListener {

	// Initialise Swing objects for GUI:
	JFrame controlFrame, outputFrame;
	JPanel controlPanel, outputPanel;
	/* Initialise text fields for: the size of the environment (one side of the 
	square) in metres, the total number of timesteps to run for, the number of 
	agents in the simulation, the filepath to where any output quantitative data 
	should be stored, the filepath containing any required room files, the 
	total number of possible activities that can be performed...
	*/
	JTextField sizeInput, loopInput, entInput, dirInput, roomInput, nactsInput;
	//...the values of the model paramaters (currently configured to the model 
	// used in King et al. 2022 https://doi.org/10.1080/23249935.2021.2017510), 
	// and a value for the equilibration time. 
	JTextField occParamInput, distParamInput, desParamInput, eqTimeInput;

	// Initialise buttons.
	JButton startSim, contSim;
	JMenuBar menuBar; // Initialise the menu.
	JMenu Moptions; // Initialise the options in the menu.
	/*
	 * Initialise one section of the menu to include: 
	 * 1. whether you want to view the simulation when its running, 
	 * 2. whether you want to read a room from a file,
	 * 3. whether you want any quantitative choice data (chosen sequences, 
	 * occupancies, distances, and desirabilities and/or schedules) written to 
	 * files,
	 * 4. whether the new next destination constraint is enforced,
	 * 5. whether an equilibration time is to be used,
	 * 6. whether agent trajectories, destination occupancies over all timesteps, 
	 * destination distances for each choice made, agent schedules after each 
	 * choice, and/or destination desirabilities for each choice made should be 
	 * written as output,
	 * 7. whether you want images of the simulation at given timesteps or not.
	 */
	JCheckBoxMenuItem cbOut, readRoom, destOutput, newNextDest, eqTime;
	JCheckBoxMenuItem trajOutput, occOutput, distOutput, schedOutput, desOutput;
	JCheckBoxMenuItem outputjpg;
	/*
	 * Initialise one section of the menu to include: 
	 * 1. whether to fix the preferred speed of each agent or randomly assign it, 
	 * 2. whether to fix the mass of each agent or randomly assign it,
	 * 3. whether to fix the radius of each agent or randomly assign it.
	 */
	JCheckBoxMenuItem varySpeed, varyMass, varySize, markov;
	/*
	 * Initialise one section of the menu to include whether to read in initial
	 * positions for agents or not.
	 */
	JCheckBoxMenuItem initialPos;

	// Initialise something that makes outputting data easier.
	PrintWriter pwout, pwout1, pwout2, pwout3, pwout4, pwout5, pwout6;
	// Initialise a class for writing output to a file(s).
	FileWriter out, out1, out2, out3, out4, out5, out6;
	// Set the filename of the output file(s).
	String filename1, filename2, filename3, filename4, filename5, filename6;
	// String versions of model parameters used when reading from input file.
	String occParamString, distParamString, desParamString;
	// Name of the simulation environment.
	String roomName;
	// Random number generator used to assign the number of possible activities at 
	// each destination.  Not currently used.
	Random monopoly = new Random();

	/*
	 * Initialise variables for: 
	 * 1. whether to start a simulation (vs continue one),
	 * 2. whether there is an output window to view simulations.
	 */
	boolean start, outputState, markovModel;
	/*
	 * Initialise: 
	 * 1. the size of a grid cell in the displayed output image window,
	 * 2. the timestep number of the simulation.
	 */
	int boxSize = 4, loop;
	// Initialise the width and height of the output window, and the 
	// number of equilibration timesteps.
	int wdg, hdg, equilTime;
	// Default x/y coordinates for a destination.
	int exitxtmp = 0, exitytmp = 0;
	// Destination choice model parameters (currently configured to the model 
	// used in King et al. 2022 https://doi.org/10.1080/23249935.2021.2017510)
	public double occParam, distParam, desParam;
	// Store floor field values for each grid cell, with a grid for each destination.
	public double[][][] fstaticTemp;
	// Store distance values for each grid cell, with a grid for each destination.
	public double[][][] distTemp;
	// Note the name of the environment to be used.
	public String type;
	// x- and y-coordinates for any destinations.
	public int[] extmp;
	public int[] eytmp;
	// Colours for agents such that agents match colour of next destination.
	Color[] entColours = { Color.red, Color.blue, Color.green, Color.pink, 
		Color.orange, Color.yellow, Color.cyan, Color.magenta, Color.gray, Color.darkGray, Color.lightGray };
	// Colour the free space within a destination area in grey.
	Color occ = new Color(127, 127, 127, 127);
	// Object for the group of agents.
	IREV1 crowd;
	// Basically a list of all possible activities (represented as letters).
	char [] alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray();
	// 2D array of the possible activities at each destination.
	public char[][] possDestActs;

	// Set the radius of the destinations in centimetres.
	int destSize = 20;
	// Locate which spatial grid cells will need to be coloured as within a 
	// destination.
	int[][] occCells;
	// Should the destination areas be distinguished?
	boolean occShow = true;
	public GuiOutput() { // Start with the constructor method.
		// System.out.println("GuiOutput constructor activated.");
				
		// Create and set up the frame.
		controlFrame = new JFrame("Control Panel"); // get the frame.
		// If you close the control window, the program stops.
		controlFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// Set the size of the frame.
		controlFrame.setSize(new Dimension(1000, 1000));

		// Create and set up the panel.
		controlPanel = new JPanel();
		// Arrange components on panel, 10 rows, 2 columns with space between.
		controlPanel.setLayout(new GridLayout(10, 2, 3, 3));
		// Create options menu.
		controlFrame.setJMenuBar(createMenuBar());

		// This calls a different method which creates Input boxes on the panel.
		addInputBox();
		
		// Set the default button.
		controlFrame.getRootPane().setDefaultButton(startSim);

		// Add the panel to the window.
		controlFrame.getContentPane().add(controlPanel, BorderLayout.WEST);

		// Display the window.
		controlFrame.pack();
		controlFrame.setVisible(true);

		// Use this initial value for the "start" variable.
		start = false;
	}

	/*
	Creates a text field box for GUI.
	Requires the following input:
	1. The value to be written inside the textbox.
	2. The text describing the value in the textbox.
	*/
	private JTextField inputBox (String value, String label) {
		// Create a text field sized to hold 10 chars.
		JTextField input = new JTextField(value, 10);
		// Add a label, sitting on the left of the panel.
		JLabel lab = new JLabel(label, SwingConstants.LEFT);
		// Add these components to the panel.
		controlPanel.add(input);
		controlPanel.add(lab);
		/*
		* Create a blank border around the text label, with height above, width right,
		* height below and width left = 5.
		*/
		lab.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		return input;
	}

	// This method creates the GUI, creating the input text fields (with default 
	// values) and buttons.
	private void addInputBox() { 
		// System.out.println("Drawing input options.");
		// Size of the space.
		sizeInput = inputBox("200", "Size of Box (px)");
		// Number of agents.  Currently constant throughout simulations.
		entInput = inputBox("0", "#individuals");
		// Number of possible activities.  Should be same as the number of destinations.
		nactsInput = inputBox("4", "number of activity types");
		// Number of timesteps to run for.
		loopInput = inputBox("10", "simulation time (x 0.05s)");
		// Number of equilibration timesteps.
		eqTimeInput = inputBox("7500", "equilibration time (x 0.05s)");
		// Name of room file to be used.
		roomInput = inputBox("empty_room_200.txt", "room file name");

		// Pull out the name of the space here.
		String roomFilename = roomInput.getText();
		String[] pieces = roomFilename.split("_");
		roomName = pieces[0];
		// Busyness parameter value for destination choice model used in King et al. 2022.
		occParamInput = inputBox("0", "occupancy parameter value");
		// Distance parameter value for destination choice model used in King et al. 2022.
		distParamInput = inputBox("0", "distance parameter value");
		// Desirability parameter value for destination choice model used in King 
		// et al. 2022.
		desParamInput = inputBox("0", "desire parameter value");		
		
		// Make the buttons.
		startSim = new JButton("Start");
		startSim.setActionCommand("start");
		contSim = new JButton("Cont.");
		contSim.setActionCommand("cont");

		// Listen to events from the buttons.
		startSim.addActionListener(this);
		contSim.addActionListener(this);

		// Add these buttons to the container
		controlPanel.add(startSim);
		controlPanel.add(contSim);
	}

	// Creates a checkbox menu for other simulation options.
	public JMenuBar createMenuBar() {
		// System.out.println("Drawing menu bar.");
		// Create the menu bar.
		menuBar = new JMenuBar();

		// Build the first menu.
		Moptions = new JMenu("Options");
		Moptions.setMnemonic(KeyEvent.VK_A);
		Moptions.getAccessibleContext().setAccessibleDescription("Menu with items");
		menuBar.add(Moptions);

		// A group of check box menu items with default values.
		// Display simulation graphics window?
		cbOut = new JCheckBoxMenuItem("Output Monitor", true);
		cbOut.setMnemonic(KeyEvent.VK_O);
		Moptions.add(cbOut);
		Moptions.addSeparator();
		// Read from a room file?
		readRoom = new JCheckBoxMenuItem("Use room from file", true);
		readRoom.setMnemonic(KeyEvent.VK_O);
		Moptions.add(readRoom);
		Moptions.addSeparator();
		// Set initial positions for agents?
		initialPos = new JCheckBoxMenuItem("initial positions", false);
		initialPos.setMnemonic(KeyEvent.VK_O);
		Moptions.add(initialPos);
		// Allow preferred speeds of agents to vary?
		varySpeed = new JCheckBoxMenuItem("vary speeds", false);
		varySpeed.setMnemonic(KeyEvent.VK_O);
		Moptions.add(varySpeed);
		// Allow masses of agents to vary?
		varyMass = new JCheckBoxMenuItem("vary weights", false);
		varyMass.setMnemonic(KeyEvent.VK_O);
		Moptions.add(varyMass);
		// Allow radius of agents to vary?
		varySize = new JCheckBoxMenuItem("vary sizes", false);
		varySize.setMnemonic(KeyEvent.VK_O);
		Moptions.add(varySize);

		Moptions.addSeparator();

		// Include Markov component in destination choice model?
		markov = new JCheckBoxMenuItem("Include Markov model?", false);
		markov.setMnemonic(KeyEvent.VK_O);
		Moptions.add(markov);
		// New next destination constraint?
		newNextDest = new JCheckBoxMenuItem("Enforce new next destinations?", true);
		newNextDest.setMnemonic(KeyEvent.VK_O);
		Moptions.add(newNextDest);
		// Use the given equilibration time?
		eqTime = new JCheckBoxMenuItem("Equilibration time?", false);
		eqTime.setMnemonic(KeyEvent.VK_O);
		Moptions.add(eqTime);

		Moptions.addSeparator();

		// Create agent destination sequence output?
		destOutput = new JCheckBoxMenuItem("Output destination sequences?", false);
		destOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(destOutput);
		// Create agent trajectory output?
		trajOutput = new JCheckBoxMenuItem("Output position trajectories?", false);
		trajOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(trajOutput);
		// Create destination occupancy output?
		occOutput = new JCheckBoxMenuItem("Output destination occupancies?", false);
		occOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(occOutput);
		// Create destination distance output?
		distOutput = new JCheckBoxMenuItem("Output destination distances?", false);
		distOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(distOutput);
		// Create destination desirability output?
		desOutput = new JCheckBoxMenuItem("Output destination desirabilities?", false);
		desOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(desOutput);
		// Create agent schedule history output?
		schedOutput = new JCheckBoxMenuItem("Output agent schedules?", false);
		schedOutput.setMnemonic(KeyEvent.VK_O);
		Moptions.add(schedOutput);

		Moptions.addSeparator();

		// Create image files of timesteps of the simulation?
		outputjpg = new JCheckBoxMenuItem("Output JPGs", true);
		outputjpg.setMnemonic(KeyEvent.VK_O);
		Moptions.add(outputjpg);

		return menuBar;
	}

	/*
	Here we draw to the simulation output.  Not used.
	Requires the following inputs:
	1. The current timestep of the simulation.
	2. Whether or not image files are being created.
	*/
	public void drawGUI(int index, boolean draw) {
		// System.out.println("Drawing current simulation state.");
		/*
		 * Initialise: 
		 * 1. starting x coordinate for the direction line, 
		 * 2. starting y coordinate for the direction line,
		 * 3. final x coordinate for the direction line,
		 * 4. final y coordinate for the direction line,
		 * 5. radius of circle representing agent.
		 */
		double x, y, xto, yto, rad;
		// Initialise graphic object for underlying output panel.
		Graphics2D gdg = (Graphics2D) outputPanel.getGraphics();
		// Initialise an image object for the output panel.
		BufferedImage bi;
		// Initialise the graphic object that will be used to draw into bi.
		Graphics2D bigdg;

		// Set image object width and height and allow it to have colour.
		bi = new BufferedImage(wdg, hdg, BufferedImage.TYPE_INT_RGB);
		bigdg = bi.createGraphics();
		// Set background colour of panel.
		bigdg.setColor(Color.white);
		// Wipe the panel clean.
		bigdg.clearRect(0, 0, wdg, hdg);
		// Create a rectangle at position 0, 0 with width and height.
		bigdg.fillRect(0, 0, wdg, hdg);
		// Change the current colour option to black for subsequent drawings.
		bigdg.setColor(Color.black);

		// This is to just draw position and direction of the individuals.
		// For each individual...
		for (int i = 0; i < crowd.N; i++) {
			x = crowd.people[i].x * 10; // x coordinate
			y = crowd.people[i].y * 10; // y coordinate
			xto = crowd.people[i].vx * 10; // x speed component
			yto = crowd.people[i].vy * 10; // y speed component
			rad = crowd.people[i].size * 10; // radius of agent

			// Sets the width of the line to be drawn.
			bigdg.setStroke(new BasicStroke(boxSize / 2));
			// Draw the direction line, starting at the centre and ending outside the
			// circle.
			bigdg.drawLine((int) Math.round(x * boxSize), (int) Math.round(y * boxSize),
					(int) Math.round(x * boxSize + (boxSize * xto)),
					(int) Math.round(y * boxSize + (boxSize * yto)));
			/*
				Draw outline of circle with top left corner in the top left of the box.
				The width and height of the circle = 2*radius
			*/
			bigdg.setColor(entColours[crowd.people[i].destination % entColours.length]);
			bigdg.fillOval((int) Math.round((x - rad) * boxSize), 
				(int) Math.round((y - rad) * boxSize), 
				(int) Math.round(2 * rad * boxSize), 
				(int) Math.round(2 * rad * boxSize));
		}

		// Now draw the spatial grid cells.
		// For each cell in the spatial grid...
		for (int uu = 0; uu < crowd.gridSize; uu++) {
			for (int vv = 0; vv < crowd.gridSize; vv++) {
				//...determine the coordinates of the top-left corner.
				x = boxSize * uu;
				y = boxSize * vv;
				// If the floor field == 0 at this cell...
				if (crowd.fstatic[0][uu][vv] == 0) {
					bigdg.setColor(Color.BLACK);
					//...fill the cell at that position in black
					bigdg.fillRect((int) x, (int) y, boxSize, boxSize);
				}
			}
		}
		
		// Actually draw the boundaries onto the output window.
		gdg.drawImage(bi, null, 0, 0);
		// Get rid of said graphics object now that we've drawn it.
		gdg.dispose();

		/* Here draw images of output, if necessary.  Required for making videos.
		Currently saves image of every other timestep.*/
		if (draw) {
			// Create filenames for each image file created.
			String name = String.format("%06d", index) + ".jpg";
			// Set to value in 'Output Directory' field.
			String imDir = System.getProperty("user.dir") + "/" + "Images"; 
			try {
				// Set the relative filepath for storing the output image files.
				File dir = new File(imDir);
				// Find the contents of the directory.
				File[] dirContents = dir.listFiles();
				// Extension of files to search for.
				String extension = ".jpg";
				// If this is the first timestep in the simulation...
				if (index == 1) {
					//...for each file currently present in the directory...
					for(int i = 0; i < dirContents.length;i++) {
						//...if the file is a jpg...
						if (dirContents[i].getName().endsWith(extension)) {
							//...delete it.
							dirContents[i].delete();
						}
					}
				}

				// Make new file in output directory.
				// If this is an odd-numbered timestep...
				if (index % 2 == 1) {
					//...create a corresponding file...
					File outputFile = new File(imDir + "/" + name);
					//...and write current output window image to file.
					ImageIO.write(bi, "jpg", outputFile);
				}
				
			} catch (IOException e) {
				// If reading from the file fails, tell us why.
				e.printStackTrace();
			}
		}
	}

	/*
	Here we draw to the simulation output.  Not used.
	Requires the following inputs:
	1. The current timestep of the simulation.
	2. Whether or not image files are being created.
	3. X-coordinates of all destinations.
	4. Y-coordinates of all destinations.
	5. Whether or not to display the destination areas in the image.
	*/
	public void drawGUI(int index, boolean draw, int[] exitx, int[] exity, boolean occShow) {
		// System.out.println("Drawing current simulation state.");
		/*
		 * Initialise: 
		 * 1. starting x coordinate for the direction line, 
		 * 2. starting y coordinate for the direction line,
		 * 3. final x coordinate for the direction line,
		 * 4. final y coordinate for the direction line,
		 * 5. radius of circle representing agent.
		 */
		double x, y, xto, yto, rad;
		// Initialise graphic object for underlying output panel.
		Graphics2D gdg = (Graphics2D) outputPanel.getGraphics();
		// Initialise an image object for the output panel.
		BufferedImage bi;
		// Initialise the graphic object that will be used to draw into bi.
		Graphics2D bigdg;

		// Set image object width and height and allow it to have colour.
		bi = new BufferedImage(wdg, hdg, BufferedImage.TYPE_INT_RGB);
		bigdg = bi.createGraphics();
		// Set background colour of panel.
		bigdg.setColor(Color.white);
		// Wipe the panel clean.
		bigdg.clearRect(0, 0, wdg, hdg);
		// Create a rectangle at position 0, 0 with width and height.
		bigdg.fillRect(0, 0, wdg, hdg);
		// Change the current colour option to black for subsequent drawings.
		bigdg.setColor(Color.black);
		// Scale the height and width of subsequent drawings by the size of the 
		// spatial grid.
		int imScaleWidth = wdg / crowd.gridSize;
		int imScaleHeight = hdg / crowd.gridSize;
		// This is to just draw position and direction of the individuals
		for (int i = 0; i < crowd.N; i++) {
			x = crowd.people[i].x * 10; // x coordinate
			y = crowd.people[i].y * 10; // y coordinate
			xto = crowd.people[i].vx * 10; // x speed component
			yto = crowd.people[i].vy * 10; // y speed component
			rad = crowd.people[i].size * 10; // radius of agent

			// Sets the width of the lines to be drawn.
			bigdg.setStroke(new BasicStroke(Math.round(boxSize / 2)));
			// Draw the direction line, starting at the centre and ending outside the
			// circle.
			bigdg.setColor(Color.BLACK);
			bigdg.drawLine((int) Math.round(x * imScaleWidth), 
			(int) Math.round(y * imScaleHeight),
			(int) Math.round(x * imScaleWidth + (imScaleWidth * xto)),
			(int) Math.round(y * imScaleHeight + (imScaleHeight * yto)));
			/*
				Draw outline of circle with top left corner in the top left of the box.
				The width and height of the circle = 2*radius
			*/
			// Colour circles according to destination.
			bigdg.setColor(entColours[crowd.people[i].destination % entColours.length]);
			bigdg.fillOval((int) Math.round((x - rad) * imScaleWidth), 
			(int) Math.round((y - rad) * imScaleHeight),
			(int) Math.round(2 * rad * imScaleWidth), 
			(int) Math.round(2 * rad * imScaleHeight));
		}
		
		// Initialise destination index.
		int exitIndex = 0;
		// For each cell in the spatial grid...
		for (int uu = 0; uu < crowd.gridSize; uu++) {
			for (int vv = 0; vv < crowd.gridSize; vv++) {
				//...rescale the coordinates of the top-left corner.
				x = imScaleWidth * uu;
				y = imScaleHeight * vv;
				// Include this if you want to see the size of destinations in space.
				// It does slow down the GUI considerably, so use it sparingly.
				if (occShow) {
					// If the current cell is part of a destination area...
					if (occCells[uu][vv] == 1) {
						// Colour it in grey.
						bigdg.setColor(occ);
						bigdg.fillRect((int) x, (int) y, imScaleWidth, imScaleHeight);
					}
				}

				// if the cell is an obstacle...
				if (crowd.fstatic[0][uu][vv] == 0) {
					/*if (uu == exitx[exitIndex] && vv == exity[exitIndex]) {
						System.out.println(uu + ", " + vv);
					}*/
					//...colour it in black.
					bigdg.setColor(Color.BLACK);
					bigdg.fillRect((int) x, (int) y, imScaleWidth, imScaleHeight);
				}

				// If the cell is a destination...
				if (uu == exitx[exitIndex] && vv == exity[exitIndex]) {
					//...give the cell a colour such that each destination a different colour.
					bigdg.setColor(entColours[exitIndex % entColours.length]);
					bigdg.fillRect((int) x, (int) y, imScaleWidth, imScaleHeight);
					exitIndex = (exitIndex + 1) % exitx.length;
				}
			}
		}

		// Actually draw the boundaries onto the output window.
		gdg.drawImage(bi, null, 0, 0);
		// Get rid of said graphics object now that we've drawn it.
		gdg.dispose();

		/* Here draw images of output, if necessary.  Required for making videos.
		Currently saves image of every other timestep.*/
		if (draw) {
			// Create filenames for each image file created.
			String name = String.format("%06d", index) + ".jpg";
			// Set target directory.
			String imDir = System.getProperty("user.dir") + "/" + "Images"; 
			// Make sure that directory exists if it doesn't already.
			new File(imDir).mkdirs();
			/* Try to delete any previous .jpg files from directory before 
			making new ones.  This is necessary when using the Matlab script 
			which creates the video later.*/
			try {
				// Set the relative filepath for storing the output image files.
				File dir = new File(imDir);
				// List all files in this directory.
				File[] dirContents = dir.listFiles();
				// Extension of files to search for.
				String extension = ".jpg";
				// If this is the first timestep in the simulation...
				if (index == 1) {
					//...for each file currently present in the directory...
					for(int i = 0; i < dirContents.length;i++) {
						//...if the file is a jpg...
						if (dirContents[i].getName().endsWith(extension)) {
							//...delete it.
							dirContents[i].delete();
						}
					}
				}

				// Make new file in output directory.
				// If this is an odd-numbered timestep...
				if (index % 2 == 1) {
					//...create a corresponding file...
					File outputFile = new File(imDir + "/" + name);
					//...and write the current output window image to file.
					ImageIO.write(bi, "jpg", outputFile);
				}
			} catch (IOException e) {
				// If image capturing fails, tell us why.
				e.printStackTrace();
			}
		}
	}

	/* This method draws the output window to screen.
	Require the following input:
	1. The size of the output window (one side of a square).
	*/
	public boolean createGUI(int noTemp) {
		// System.out.println("Drawing output window.");
		// Set title of output window.
		outputFrame = new JFrame("Output Window");
		// Close the window when you press the x.
		outputFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		// Set height and width of output window.
		wdg = noTemp * boxSize;
		hdg = noTemp * boxSize;

		outputPanel = new JPanel();
		outputPanel.setPreferredSize(new Dimension(wdg, hdg));

		// Set the location on the screen where the output window will appear.
		outputFrame.setLocation(450, 0);
		outputFrame.getContentPane().add(outputPanel);
		outputFrame.pack();
		// Make the output window appear.
		outputFrame.setVisible(true);

		return true;
	}

	/* Basically run the simulation when 'start' or 'continue' buttons clicked.
	Requires the following input:
	1. An 'event' produced when a button is clicked.
	*/
	public void actionPerformed(ActionEvent event) {
		/*
		 * System.out.println("Performing action based on command " +
		 * event.getActionCommand() + ".");
		 */
		// Provide default values for the size of the spatial grid and the number 
		// of agents present.
		int sizeTemp = 150, bugsTemp = 1;
		// Set strings to hold the number of timesteps, the name of the room, the 
		// size of the spatial grid, and the number of agents present.
		String loopStr, roomStr, sizeStr, nentStr;
		// If an action = the wakeup action...
		if ("wakeup".equals(event.getActionCommand())) {

		}
		// Else if the action = the okay action...
		else if ("ok".equals(event.getActionCommand())) {

		}
		// Otherwise, if the 'Start' or 'Continue'button is pressed...
		else {
			//...initialise the position of the destinations for the room.
			exitxtmp = -10;
			exitytmp = -10;
			// Read in the value in the 'Loops' box...
			loopStr = loopInput.getText();
			// ...and turn it into a number.
			loop = ((Number) Double.parseDouble(loopStr)).intValue();
			// If the equilibration time is to be used...
			if (eqTime.getState()) {
				//...read in the value in the equilibration time box...
				String eqTimeIn = eqTimeInput.getText();
				// ...and turn it into a number.
				equilTime = ((Number) Integer.parseInt(eqTimeIn)).intValue();
			}
			// Otherwise, if an equilibration time is not being used...
			else {
				//...then there is no equilibration time.
				equilTime = 0;
			}
			// If we started before...
			if (("start".equals(event.getActionCommand())) || (start == false)) {
				//...and if an output window is being drawn...
				if (outputState) {
					//...remove the output window from the screen...
					outputFrame.dispose();
					//...and reset the output state to false.
					outputState = false;
				}

				// Default value for the room name, which should be contained within a 
				// room file name.
				type = "empty";
				// If the 'Use room from file' option from the menu is selected...
				if (readRoom.getState()) {
					//...read in the value in the 'room file name' box.
					roomStr = roomInput.getText();
					// Try to connect to room file and read in values.
					try {
						FileInputStream fstream = new FileInputStream("Rooms/" + roomStr);
						// Get the object of DataInputStream
						DataInputStream in = new DataInputStream(fstream);
						BufferedReader br = new BufferedReader(new InputStreamReader(in));

						// Read in which room you're using from the room file.
						type = br.readLine();
						// Read in the size of the space from the room file.
						int gridSize = Integer.valueOf(br.readLine()).intValue();
						// Read in the number of destinations from the room file.
						int numDest = Integer.valueOf(br.readLine()).intValue();
						possDestActs = new char[numDest][];
						// Initialise the floor fields.
						fstaticTemp = new double[numDest][gridSize][gridSize];
						// Initialise distances from each destination.
						distTemp = new double[numDest][gridSize][gridSize];
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

						// Read in static floor field:
						int count1 = 0; // line/row counter
						int count2 = 0; // element/column counter
						String line = br.readLine();
						// Separates contents of the line around the ','.
						String[] parts;
						int fieldIndex = 0; // what field we are on
						// Over the entire number of floor fields expected...
						while (fieldIndex < numDest) {
							//...cycle over the spatial grid.
							while (count1 <= (gridSize - 1)) {
								//...and split the line by commas.
								parts = line.split(",");
								// Assign floor field values now.
								fstaticTemp[fieldIndex][count1][count2] = Double.valueOf(parts[count2]).doubleValue();
								count2 += 1; // increment column index
								// If we reach the end of a row...
								if (count2 == (gridSize)) {
									// ...read in the next line...
									line = br.readLine();
									// ...reset the column index...
									count2 = 0;
									// ...and increment the row index.
									count1 += 1;
								}
							}
							// Now do the same thing described above for the distance values.
							count1 = 0;
							count2 = 0;
							// While loops done in this order to reflect room file structure.
							while (count1 <= (gridSize - 1)) {
								parts = line.split(",");
								// Assign floor field values now.
								distTemp[fieldIndex][count1][count2] = Double.valueOf(parts[count2]).doubleValue();
								// fstaticTemp[count1][count2] = Double.valueOf(br.readLine()).doubleValue();
								count2 += 1; // increment column index
								// If we reach the end of a row...
								if (count2 == (gridSize)) {
									// ...read in the next line...
									line = br.readLine();
									// ...reset the column index...
									count2 = 0;
									// ...and increment the row index.
									count1 += 1;
								}
							}
							// Increment field index without it going out of bounds.
							fieldIndex++;
							// Reset both row and column indices.
							count1 = 0;
							count2 = 0;
						}
						// Need to assign the right value for the environment size:
						sizeTemp = gridSize;
						br.close();
					} catch (IOException e) {
						// If reading in the room file fails, tell us why.
						e.printStackTrace();
					}
				} 
				// Otherwise, if we are not reading an environment from a file...
				else {
					//...the default spatial grid dimension is 200 cells.
					sizeTemp = 200;
					// Set the coordinates for all destinations.
					extmp = new int[4];
					eytmp = new int[4];
					// For each destination...
					for (int yy = 0; yy < 4; yy++) {
						//...they all have the same coordinates.
						extmp[yy] = exitxtmp;
						eytmp[yy] = exitytmp;
					}
				}
				// Read in window size from panel and turn into number.
				sizeStr = sizeInput.getText();
				int winSize = ((Number) Double.parseDouble(sizeStr)).intValue();
				// Read in number of individuals from panel and turn it into a number.
				nentStr = entInput.getText();
				bugsTemp = ((Number) Double.parseDouble(nentStr)).intValue();
				// Read in number of possible activities...
				String acts = nactsInput.getText();
				// ...and turn it into a number.
				int numAct = ((Number) Integer.parseInt(acts)).intValue();
				
				// Create the array of possible activities.
				// Take first numAct elements of alphabet as possible activities.
				char [] allActsArr = Arrays.copyOfRange(alphabet, 0, numAct);
				// Make a mutable copy of possible activities.
				ArrayList<Character> allActs = new ArrayList<Character>();
				for (int i = 0; i < allActsArr.length; i++) {
					allActs.add(allActsArr[i]);
				}

				// For each destination...
				for (int i = 0; i < possDestActs.length; i++) {
					//...state that only one activity can be performed.
					char [] destAct = new char[1]; // hard-coded length 1
					//...which is corresponding entry from possible activities.
					destAct[0] = allActsArr[i];
					// Collate all activities for this destination into structure holding 
					// activities for all destinations.
					possDestActs[i] = destAct;
				}

				// Extract the values of the destination choice model parameters 
				// (currently the model used in King et al. 2022)...
				occParamString = occParamInput.getText();
				distParamString = distParamInput.getText();
				desParamString = desParamInput.getText();
				//...and convert them into numbers.
				occParam = ((Number) Double.parseDouble(occParamString)).doubleValue();
				distParam = ((Number) Double.parseDouble(distParamString)).doubleValue();
				desParam = ((Number) Double.parseDouble(desParamString)).doubleValue();
				// Set directory for output files.
				String dir = System.getProperty("user.dir") + "/" + roomName 
				+ "_occ_param=" + occParam + "_dist_param=" + distParam 
				+ "_des_param=" + desParam;
				// Make directory if it doesn't already exist.
				new File(dir).mkdirs();
				// Now state the filenames of each output WITHOUT making the files.
				// Entity trajectories.
				filename1 = "busy=" + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_ent_trajs_" 
				+ bugsTemp + "_ents_sim_1.txt";
				// Destination occupancies.
				filename2 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_occ_"
				+ bugsTemp + "_ents_sim_1.txt";
				// Chosen destination sequences.
				filename3 = "busy=" + occParam + "_dist=" +
				distParam + "_des=" + desParam + "_dest_seq_" 
				+ bugsTemp + "_ents_sim_1.txt";
				// Destination distances.
				filename4 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_dist_"
				+ bugsTemp + "_ents_sim_1.txt";
				// Destination desirabilities.
				filename5 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_dest_des_"
				+ bugsTemp + "_ents_sim_1.txt";
				// Entity schedules.
				filename6 = "busy=" + + occParam + "_dist=" + 
				distParam + "_des=" + desParam + "_ent_sched_"
				+ bugsTemp + "_ents_sim_1.txt";

				// Setup the grapher frame if the 'Output Monitor' option is selected.
				if (cbOut.getState()) {
					outputState = createGUI(winSize);
				}
				
				// Initialise output to files:
				
				try {
					// If entity trajectories are to be outputted...
					if (trajOutput.getState()) {
						// Create output file in stated directory.
						out1 = new FileWriter(dir + "/" + filename1);
						// Create a writer object ready to write to said file.
						pwout1 = new PrintWriter(out1);
					}
					// If destination occupancies are to be outputted...
					if (occOutput.getState()) {
						// Create output file in stated directory.
						out2 = new FileWriter(dir + "/" + filename2);
						// Create a writer object ready to write to said file.
						pwout2 = new PrintWriter(out2);
					}
					// If chosen destination sequences are to be outputted...
					if (destOutput.getState()) {
						// Create output file in stated directory.
						out3 = new FileWriter(dir + "/" + filename3);
						// Create a writer object ready to write to said file.
						pwout3 = new PrintWriter(out3);
					}
					// If destination distances are to be outputted...
					if (distOutput.getState()) {
						// Create output file in stated directory.
						out4 = new FileWriter(dir + "/" + filename4);
						// Create a writer object ready to write to said file.
						pwout4 = new PrintWriter(out4);
					}
					// If destination desirabilities are to be outputted...
					if (desOutput.getState()) {
						// Create output file in stated directory.
						out5 = new FileWriter(dir + "/" + filename5);
						// Create a writer object ready to write to said file.
						pwout5 = new PrintWriter(out5);
					}
					// If entity schedules are to be outputted...
					if (schedOutput.getState()) {
						// Create output file in stated directory.
						out6 = new FileWriter(dir + "/" + filename6);
						// Create a writer object ready to write to said file.
						pwout6 = new PrintWriter(out6);
					}
											
				} catch (IOException e) {
					// If any of these fail, tell us why.
					e.printStackTrace();
				}
				
				// If the 'Markov model' option in the menu is selected...
				if (markov.getState()) {
					//...then include the Markov model.
					markovModel = true;
				}
				// If the 'vary speeds' option in the menu is selected...
				if (varySpeed.getState()) {
					// ...then give the individuals variable speeds.
					crowd.varySpeed = true;
				}
				// If the 'vary weights' option in the menu is selected...
				if (varySize.getState()) {
					// ...then give the individuals variable masses.
					crowd.varySize = true;
				}
				// If the 'vary sizes' option in the menu is selected...
				if (varyMass.getState()) {
					// ...then give the individuals variable radii.
					crowd.varyMass = true;
				}

				// Declare the main simulation.

				// Set model parameters.
				double [] params = {occParam, distParam, desParam};

				// If Markov model included...
				if (markovModel) {
					// Try to assign transition matrix from file.
					double[][] transMat = new double[extmp.length][extmp.length];
					try {
						FileInputStream fstream = new FileInputStream(
							dir + "/trans_mat.txt");
						// Get the object of DataInputStream
						DataInputStream in = new DataInputStream(fstream);
						BufferedReader br = new BufferedReader(new InputStreamReader(in));
						// For each row of the matrix/line of the file...
						for (int i = 0; i < extmp.length; i++) {
							//...read in the row.
							String line = br.readLine();
							// Separates contents of line around the ','.
							String[] parts = line.split(",");
							// For each value in the row...
							for (int j = 0; j < parts.length; j++) {
								//...set corresponding element of matrix.
								transMat[i][j] = Double.valueOf(parts[j]);
							}
						}
						br.close(); // close connection.
					} 
					// If this fails for some reason, make a new matrix.
					catch (IOException e) {
						// Tell user what has happened.
						System.out.println("Transition matrix doesn't exist.");
						// Normalisation constant for elements of transition matrix.
						double normConst = 0;
						// New random number generator for choosing matrix elements.
						Random hedbanz = new Random();
						// Assign the filename for the transition matrix file.
						String filename = dir + "/trans_mat.txt";
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
							normConst = 0;
							// For each value in the row...
							for (int c = 0; c < extmp.length; c++) {
								// Matrix must be symmetric.
								if (c >= r) {
									transMat[r][c] = hedbanz.nextDouble();
									transMat[c][r] = transMat[r][c];
								}
								// Increment normalisation constant for the row.
								normConst += transMat[r][c];
							}
							// Now divide each value by said constant.
							for (int c = 0; c < extmp.length; c++) {
								transMat[r][c] /= normConst;
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
					/* Create the crowd of bugsTemp agents in a spatial grid of dimension 
					sizeTemp, with destinations with coordinates (extmp, eytmp), params 
					destination	choice model parameters, the specified transition matrix,
					the	possible activities that can be performed, and the activities that 
					can be performed at each destination.*/
					crowd = new IREV1(bugsTemp, sizeTemp, extmp, eytmp, params, transMat, allActs, possDestActs, newNextDest.getState());
				}
				// Otherwise, if the Markov model is not included...
				else {
					/* Create the crowd of bugsTemp agents in a spatial grid of dimension 
					sizeTemp, with destinations with coordinates (extmp, eytmp), params 
					destination	choice model parameters, the	possible activities that 
					can be performed, and the activities that can be performed at each 
					destination.*/
					crowd = new IREV1(bugsTemp, sizeTemp, extmp, eytmp, params, allActs, possDestActs, newNextDest.getState());
				}
				// Here assign the correct static floor field from the room file:
				// If the environment has been read in from a file...
				if (readRoom.getState()) {
					//...for each destination...
					for (int mm = 0; mm < extmp.length; mm++) {
						//...for each row in the spatial grid...
						for (int ll = 0; ll < crowd.gridSize; ll++) {
							//...for each value in the row...
							for (int kk = 0; kk < crowd.gridSize; kk++) {
								//...set the values of the floor field and distance from the 
								// given destination for this cell.
								crowd.fstatic[mm][ll][kk] = fstaticTemp[mm][ll][kk];
								crowd.distances[mm][ll][kk] = distTemp[mm][ll][kk];
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
				
				// If initial positions of agents are provided...
				if (initialPos.getState()) {
					//...then try to read then in from the input file.
					try {
						String parafile = "1";
						// Create file input object for file in 'parafiles' directory.
						FileInputStream fstream = new FileInputStream("parafiles/" + parafile);
						// Get the object of DataInputStream
						DataInputStream in = new DataInputStream(fstream);
						BufferedReader br = new BufferedReader(new InputStreamReader(in));
						// Skip the first 10 lines in the file which detail simulation conditions.
						for (int u = 0; u < 10; u++) {
							parafile = br.readLine();
						}
						/*
						 * After the first 10 lines, read in: x, y, mass, radius
						 * and preferred speed for each agent from file.  Order of for loops 
						 * should match file format.
						 */
						// For each agent...
						for (int u = 0; u < bugsTemp; u++) {
							//...read in the x-coordinate of the agent.
							crowd.people[u].x = Double.valueOf(br.readLine()).doubleValue();
						}
						// For each agent...
						for (int u = 0; u < bugsTemp; u++) {
							//...read in the y-coordinate of the agent.
							crowd.people[u].y = Double.valueOf(br.readLine()).doubleValue();
						}
						// For each agent...
						for (int u = 0; u < bugsTemp; u++) {
							//...read in the mass of the agent.
							crowd.people[u].mass = Double.valueOf(br.readLine()).doubleValue();
						}
						// For each agent...
						for (int u = 0; u < bugsTemp; u++) {
							//...read in the radius of the agent.
							crowd.people[u].size = Double.valueOf(br.readLine()).doubleValue();
						}
						// For each agent...
						for (int u = 0; u < bugsTemp; u++) {
							//...read in the preferred speed of the agent.
							crowd.people[u].speed = Double.valueOf(br.readLine()).doubleValue();
						}
						br.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
					// Tell the crowd object that initial conditions already given.
					crowd.setInitialPosgiven();
				} 
				// Otherwise, if initial conditions not provided...
				else {
					//...assign initial conditions of agents:
					crowd.setInitial(0);
				}
				// The simulation is now ready to start.
				start = true;
			}

			// If we want agent trajectory output...
			if (trajOutput.getState()) {
				// Write out information about the simulation.
				pwout1.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			// If we want destination occupancy output...
			if (occOutput.getState()) {
				// Write out information about the simulation.
				pwout2.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			// If we want destination sequence output...
			if (destOutput.getState()) {
				// Write out information about the simulation.
				pwout3.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			// If we want destination distance output...
			if (distOutput.getState()) {
				// Write out information about the simulation.
				pwout4.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			if (desOutput.getState()) {
				// Write out information about the simulation.
				pwout5.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			// If we want agent schedule history output...
			if (schedOutput.getState()) {
				// Write out information about the simulation.
				pwout6.println("type = " + type + ", gridSize = " + 
				crowd.gridSize + ", steps = " + loop + 
				", Number of destinations = " + extmp.length + ", N = " 
				+ crowd.N + ", occupancy parameter = " + occParam 
				+ ", distance parameter = " + distParam 
				+ ", desirability parameter = " + desParam);
			}
			// If the Markov model is included...
			if (markov.getState() && destOutput.getState()) {
				/* Write each row of the transition matrix as a line of 
				comma-separated values in the destination sequence output file.
				For each row of the transition matrix...*/
				for (int i = 0; i < crowd.transMat.length; i++) {
					//...for each column of the row...
					for (int j = 0; j < crowd.transMat[i].length; j++) {
						//...write the value to the current line.  Separated by commas.
						pwout3.print(crowd.transMat[i][j] + ",");
					}
					pwout3.println();
				}
			}
			// Now determine which spatial grid cells lie within destination areas.
			occCells = new int[crowd.gridSize][crowd.gridSize];
			// For each destination...
			for (int ww = 0; ww < crowd.distances.length; ww++) {
				//...for each row of the spatial grid...
				for (int uu = 0; uu < crowd.gridSize; uu++) {
					//...for each value of the row...
					for (int vv = 0; vv < crowd.gridSize; vv++) {
						//...if the distance value is less than the size of the given 
						// destination...
						if (crowd.distances[ww][uu][vv] <= destSize) {
							//...then the cell is part of the destination area.
							occCells[uu][vv] = 1;
						}
					}
				}
			}

			//////////////////////////////
			// here perform simulation: //
			//////////////////////////////
			// Do we want to show the output window?
			boolean pics = outputjpg.getState();
			// Initialise timestep index.
			int i = 0;
			/*
			 * Now run each timestep/loop of the simulation. While we've 
			 * performed less than the desired number of timesteps.
			 */
			while (i < loop) {
				// Increment the timestep.
				i += 1;

				// Update the positions of the agents.
				crowd.updateMovement(i);
				// Update the choices of the agents, where necessary.
				crowd.updateChoices(i);
				// If the 'Output Monitor' option is selected, draw the output window.
				if (outputState) {

					// If you want to have a delay between output frames.
					/*
					try {Thread.sleep(20); }// in milliseconds.  
					catch(InterruptedException ex)
					{ Thread.currentThread().interrupt(); }
					*/
					// If image files are to be generated...
					if (outputjpg.getState()) {
						//...and this is an odd-numbered timestep...
						if (i % 1 == 0) {
							//...then an image of the simulation at this timestep should be 
							// written to a file.
							pics = true;
						}
						// Otherwise, if this is not an odd-numbered timestep...
						else {
							//...then an image of the simulation at this timestep should not 
							// be written to a file.
							pics = false;
						}
					}
					// Draw the simulation output window.
					drawGUI(i, pics, extmp, eytmp, occShow);
				}

				// If we want agent trajectory output...
				if (trajOutput.getState()) {
					//...if an equilibration time is being used...
					if (eqTime.getState()) {
						//...then first check that the current timestep is after the 
						// equilibration time, if it is...
						if (i >= equilTime) {
							//...then for every agent...
							for (int u = 0; u < bugsTemp; u++) {
								//...record the x positions of the agent.
								pwout1.print(crowd.people[u].x + ",");
							}
						}
					}
					// Start a new line.
					pwout1.println();
					// Now print the corresponding y-positions of every agent.
					// For every agent...
					for (int u = 0; u < bugsTemp; u++) {
						//...record the y positions of the agent.
						pwout1.print(crowd.people[u].y + ",");
					}
					// Start a new line.
					pwout1.println();
				}
				// If we want destination occupancy output...
				if (occOutput.getState()) {
					//...if an equilibration time is being used...
					if (i >= equilTime) {
						//...for each destination...
						for (int u = 0; u < crowd.destPopularity.length; u++) {
							//...record the occupancy at this timestep.
							pwout2.print(crowd.destPopularity[u] + ",");
						}
						// Start a new line for next timestep.
						pwout2.println();
					}
				}
			}
		
			// If we want destination sequence output...
			if (destOutput.getState()) {
				//...write agent's decision times and destination sequences to file.
				crowd.writeDestSequence(pwout3, equilTime);
				// Try to close the connection.
				try {
					out3.close();
					pwout3.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
			// If we have agent trajectory output...
			if (trajOutput.getState()) {
				// Try to close the connection.
				try {
					out1.close();
					pwout1.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
			// If we have destination occupancy output...
			if (occOutput.getState()) {
				// Try to close the connection.
				try {
					out2.close();
					pwout2.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
			// If we want destination distance output...
			if (distOutput.getState()) {
				//...write destination distances to file.
				crowd.writeDistances(pwout4, equilTime);
				// Try to close the connection.
				try {
					out4.close();
					pwout4.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
			// If we want destination desirability output...
			if (desOutput.getState()) {
				//...write destination desirabilities to file.
				crowd.writeDesires(pwout5, equilTime);
				// Try to close the connection.
				try {
					out5.close();
					pwout5.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
			// If we want agent schedule output...
			if (schedOutput.getState()) {
				//...write agent's schedule history to file.
				crowd.writeEntSchedHist(pwout6, equilTime);
				// Try to close the connection.
				try {
					out6.close();
					pwout6.close();
				} catch (IOException e) {
					// If closing the connection fails, tell us why.
					e.printStackTrace();
				}
			}
		}
	}

	// Start up the simulator GUI.
	private static void createAndShowGUI() {
		// System.out.println("GuiOutput activated.");
		// Make sure we have nice window decorations.
		JFrame.setDefaultLookAndFeelDecorated(true);
		// Initialise the GUI.
		new GuiOutput();

	}

	// Now let's get started!
	public static void main(String[] args) {
		// System.out.println("Begin program.");

		/* Schedule a job for the event-dispatching thread:
		creating and showing this application's GUI.*/
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});
	}
}
