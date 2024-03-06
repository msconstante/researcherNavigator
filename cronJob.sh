export OPENSSL_CONF=/dev/null 
cd /srv/researchernavigator 
date >> ./output/log.txt
nice -n 19 Rscript ./cronJob.R . >> ./output/log.txt
rm /tmp/researchernavigator.lockfile
