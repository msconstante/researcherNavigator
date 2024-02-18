export OPENSSL_CONF=/dev/null 
cd /srv/researchernavigator 
nice -n 19 Rscript ./cronJob.R .
