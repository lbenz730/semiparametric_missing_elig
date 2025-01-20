for SIM_ID in $(seq 1 8); do
for EST_ID in $(seq 1 19); do
#
echo "${SIM_ID}, ${EST_ID}"
export SIM_ID EST_ID
#
sbatch -o logs/aligned_t0/log_sim${SIM_ID}_est${EST_ID}.stdout \
-e logs/aligned_t0/log_sim${SIM_ID}_est${EST_ID}.stderr \
--job-name=at0_${SIM_ID} \
jobs/aligned_t0_sims_loop.sh
#
sleep 1 # pause to be kind to the scheduler
done
done