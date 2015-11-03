[Debug]
-- Directories only used when building with Debug configuration

*.inc = obj\debug
*.clw = obj\debug
*.exp = obj\debug
*.shp = obj\debug
*.map = obj\debug

[Release]
-- Directories only used when building with Release configuration

*.inc = obj\release
*.clw = obj\release
*.exp = obj\release
*.shp = obj\release
*.map = obj\release

{include %bin%\%REDNAME%}

[Common]

*.clw = ..\_classes; _classes
*.inc = ..\_classes; _classes
*.ico = _resources
*.gif = _resources
*.cur = _resources
*.jpg = _resources