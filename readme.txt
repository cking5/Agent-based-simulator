This repository contains all necessary code to run the agent-based simulator used in King et al. 2022 (https://doi.org/10.1080/23249935.2021.2017510).

The simulator is implemented in Java with three main files:
1. Initialisation - this file sets up the simulation(s) and takes in any necessary input from other files, such as environmental layout, Markov transition matrix, or general simulation settings.  This can be done through a GUI (GUIOutput.java) or through the Windows Command Line (OutputIREV.java or OutputIREV2.java).  This is where destination occupancies and agent trajectories are output.
2. Crowd object (IREV1.java) - defines the crowd of agents and its properties.  This is where outputs agent chosen destination sequences, agent schedule histories, destination distances, and destination desirabilities are created.
3. Agent object (agent.java) - defines the agent object and their properties.

The simulator is run via the following Windows commands:
'javac initialisationfilename.java'
then:
'java initialisationfilename anynecessaryinputs'

The first command compiles the all the code (the initialisation file uses the crowd and agent objects).  The second command runs the simulator, remember to include any necessary inputs, such as the name of a settings file.

To create simulation environments, use the create_fstatic.R file.  This contains functions to create a variety of environmental layouts, such as those featured in King et al. 2022.  It can be run independently of the simulator and stores any output files in the 'Rooms' directory.

If running using the GUI, then the user specifies the simulation conditions using the GUI.  The GUI does not require an environmental layout file.  From the command line, the simulator requires these conditions in the form of a settings file, which chould be a text file of the same structure as 'initial.txt' found in parafiles.  The simulator naturally expects settings files to be stored in this directory, if not specified by the user in the command line.  'notes.txt' provides details of the information contained within a settings file.  Running the simulation from the command line requires an environmental layout file which should have the same layout as the text files currently included in 'Rooms'.

When running through the GUI, there is an option to generate image files at various stages of the simulation.  These can be used to create MP4 videos using the Matlab file 'makefilm.m'.

Further details of how the simulator works can be found in my thesis (link here) in Section 2.2.1.

Hopefully each file is adequately documented, but let me know if anything is not clear.