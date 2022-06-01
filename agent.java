import java.util.*;
/* This is a crucial part of the agent-based simulator used in King et. al 2022 
https://doi.org/10.1080/23249935.2021.2017510.  More details about how the 
simulator was used can be found there. 

This script defines the agent object for pedestrian simulator.
*/
public class agent {

	public double x; // current x position
	public double y; // current y position
	public double vx; // current speed x component
	public double vy; // current speed y component
	public double xnew; // new x position
	public double ynew; // new y position
	public double vxnew; // new speed x component
	public double vynew; // new speed y component
	public int destination; // where is entity going?
	//public boolean updateDest; // Has the entity changed destination?
	public Random cluedo; // for random fixed schedule assignment
	public ArrayList<Character> schedule; // sequence of desired activities
	// Sequence of destinations chosen and the corresponding decision times.
	public ArrayList<Integer> dests, decisionTimes;
	/* List of distances to each destination from the agent's current position
	at decision time.*/
	public List<List<Double>> dists;
	/* List of desirabilities of each destination from the agent's current position
	at decision time.*/
	public List<List<Double>> desires;
	// List of activity schedules after each destination decision was made.
	public List<Character[]> schedHist;
	// Time to spend at each destination.
	public int tStay;
	// The current activity to perform.
	public char currAct;
	public double speed; // preferred speed
	public double size; // radius
	public double mass; // mass
	// A parameter from Helbing et. al 2000 https://www.nature.com/articles/35035023
	public double A;
	// B parameter from Helbing et. al 2000 https://www.nature.com/articles/35035023
	public double B;

	/* Constructor for agent.
	Requires the following inputs:
	1. The total number of destinations in the simulation.
	2. The list	of possible activities.
	3. The length of the activity schedule to be created.
	*/
	public agent(int numDest, ArrayList<Character> acts, int schedLength) {
		// Set default parameter values:
		tStay = 0;
		size = 0.2;
		speed = 0.6;
		mass = 80;
		A = 2000; // from Helbing et. al 2000 https://www.nature.com/articles/35035023
		B = 0.08; // from Helbing et. al 2000 https://www.nature.com/articles/35035023
		// Initialise the lists.
		dests = new ArrayList<>();
		decisionTimes = new ArrayList<>();
		// Each item of these lists are always the number of destinations long.
		dists = new ArrayList<List<Double>>(numDest);
		desires = new ArrayList<List<Double>>(numDest);
		schedHist = new ArrayList<Character[]>();
		// Random number generator for assigning first destination.
		cluedo = new Random();

		// Initialise the activity schedule as a random sequence of all possible 
		// activities where each activity appears only once.
		schedule = new ArrayList<Character>(schedLength);
		ArrayList<Character> actsCopy = (ArrayList<Character>) acts.clone();
		Collections.shuffle(actsCopy);
		schedule = actsCopy;
		
		// This generates a schedule which are repeating sequences of activities.
		// For each entry in the activity schedule...
		/*for (int k = 0; k < schedLength; k++) {
			// Schedule is random.
			schedule.add(acts.get(cluedo.nextInt(numActs)));
			// Schedule is repeating sequence of all activities.
			//schedule.add(acts.get(k % numActs));
			//schedule.add(acts.get(k));
			//System.out.println(schedule.get(k));
		}*/
		
		// Note down the initial, full activity schedule in the history.
		schedHist.add(schedule.toArray(new Character[0]));

		// Randomly assign the first destination.
		destination = cluedo.nextInt(numDest);
	}

	/*
	Method for updating the activity schedule of the agent.  Removing the 
	activity that has just been completed and writing the updated schedule to
	the agent's schedule history.  
	It requires the following input:
	1. The activity to be removed.
	*/
	public void updateSchedule(char act) {
		// Try to find the activity to be removed.
		try {
			// If you can find the activity in the schedule...
			int actLoc = this.schedule.indexOf(act);
			//...remove the activity from the appropriate place in the schedule.
			this.schedule.remove(actLoc);
			// Add this updated schedule to the schedule history.
			this.schedHist.add(this.schedule.toArray(new Character[0]));
		// If you can't find the activity, the tell us why.
		} catch(IndexOutOfBoundsException ex) {
			ex.printStackTrace();
		}
	}
}

