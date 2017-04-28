#!/usr/bin/env bash
bundle exec jekyll build --config _config.yml,_config_github.yml
# from https://github.com/rudametw/rudametw.github.io
'cp' -aRf _site/* ../ && git add ../ && git commit -m "Update site" && git push
