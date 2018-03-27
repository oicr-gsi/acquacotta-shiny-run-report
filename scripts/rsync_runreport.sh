source=/oicr/data/archive/web/runReports/*/*.tsv
destination=ubuntu@10.30.129.65:/home/ubuntu/data/acquacotta/runreport_files

rsync -azP -e "ssh -i ~/.ssh/rshiny" --delete $source $destination
