#Get variable from the command line
#This is the main directory to start at
maindir=$1

echo "Processing subdirectories in $maindir"
#cycle through all the subdirectories in a directory and find the ones that have a 'Request.rds' file
for dir in $(ls -d $maindir); do
    if [ -f $dir/request.rds ]; then
        #if results.rds exists move to next folder
        if [ -f $dir/results.rds ]; then
            echo "Results file exists in $dir"
            continue
        fi
        #Check if a running.tmp file exists
        if [ -f $dir/running.tmp ]; then
            echo "Running file exists in $dir"
            continue
        else
            #Call the rscript 'ProcessRequest.R with dir as code'
            echo "Processing $dir"
            #create a running.tmp file
            touch $dir/running.tmp
            Rscript ProcessRequest.R $dir
            #Remove running.tmp once done
            rm $dir/running.tmp
    fi
done


