#!/bin/bash


#SBATCH --nodes=1 					# Number of nodes or computers. Should always be 1 for now.
#SBATCH --ntasks=10 					# Number of CPU cores. As you request more CPU cores, you are also getting more CPU memory. You have about 3.8G per core
#SBATCH --time=36:00:00 				# Walltime
#SBATCH --qos=long 					# normal for <24 hours; mem for high mem; long for very long jobs (7 days max)
#SBATCH --partition=amilan 				# normal CPU partition.
#SBATCH --job-name=mitograph_20230324_1-65 		# Name of the job that will be submitted.
#SBATCH --output=mitograph_20230324_1-65.%j.out 	# Name of the file where all the benign outputs and logs related to the run will be redirected. %j is the variable that will capture the jobID
#SBATCH --error=mitograph_20230324_1-65.%j.err 		# Name of the file where all the errors related to the run will be redirected.
#SBATCH --mail-type=BEGIN,FAIL,END 			# I get the Slurm notification in my email inbox when it begins, ends and fails. 
#SBATCH --mail-user=kyle.ross@cuanschutz.edu 		# Whom to send the email for all the requested notifications. 
#SBATCH --array=1-65 					# Change to represent the number of jobs to be submitted e.g. 1-7 or 2-5 

cd /scratch/alpine/kross1@xsede.org/20230324_UK_pathVariants/MitoGraph_batchInput/

module use --append /pl/active/mitograph/software/lmod-files/
module load mitograph/3.1

export IndexID=$SLURM_ARRAY_TASK_ID

run_mitograph -xy 0.11 -z 0.3 -adaptive 10 -path /scratch/alpine/kross1@xsede.org/20230324_UK_pathVariants/MitoGraph_batchInput/$IndexID -labels_off