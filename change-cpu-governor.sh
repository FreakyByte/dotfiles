#!/usr/bin/env fish

set cpufreq (cpufreq-info -c 0 -p)
switch $cpufreq
        case '*ondemand*'
		sudo -A cpufreq-set -c 0 -g performance
		sudo -A cpufreq-set -c 1 -g performance
		sudo -A cpufreq-set -c 2 -g performance
		sudo -A cpufreq-set -c 3 -g performance
		sudo -A cpufreq-set -c 4 -g performance
		sudo -A cpufreq-set -c 5 -g performance
		notify-send -i /usr/share/icons/Flat-Remix-Black-Dark/emblems/scalable/vcs-locally-modified-unstaged.svg "PERFORMANCE" "CPU governors set to \"performance\""
        case '*performance'
		sudo -A cpufreq-set -c 0 -g ondemand
		sudo -A cpufreq-set -c 1 -g ondemand
		sudo -A cpufreq-set -c 2 -g ondemand
		sudo -A cpufreq-set -c 3 -g ondemand
		sudo -A cpufreq-set -c 4 -g ondemand
		sudo -A cpufreq-set -c 5 -g ondemand
		notify-send -i /usr/share/icons/Flat-Remix-Black-Dark/emblems/scalable/vcs-removed.svg "ONDEMAND" "CPU governors set to \"ondemand\""
    end
