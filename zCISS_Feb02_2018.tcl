set tcl_precision 12

# Working directory where analysis files are located
set wd [pwd]

# Directory variable required for pdf4tcl/pkgIndex.tcl
set dir .

lappend auto_path $dir
lappend auto_path $wd

if {1 || $tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
#-----------------------------------------------------------------------
# TITLE:
#	snit.tcl
#
# AUTHOR:
#	Will Duquette
#
# DESCRIPTION:
#       Snit's Not Incr Tcl, a simple object system in Pure Tcl.
#
#       Copyright (C) 2003-2005 by William H. Duquette
#       This code is licensed as described in license.txt.
#
#-----------------------------------------------------------------------

package provide snit 1.0

#-----------------------------------------------------------------------
# Namespace

namespace eval ::snit:: {
    namespace export          compile type widget widgetadaptor typemethod method macro
}

#-----------------------------------------------------------------------
# Some Snit variables

namespace eval ::snit:: {
    variable reservedArgs {type selfns win self}

    # If true, get a pretty, fixed-up stack trace.  Otherwise, get raw
    # stack trace.
    # NOTE: Not Yet Implemented
    variable prettyStackTrace 1
}

#-----------------------------------------------------------------------
# Snit Type Implementation template

namespace eval ::snit:: {
    # Template type definition: All internal and user-visible Snit
    # implementation code.
    #
    # The following placeholders will automatically be replaced with
    # the client's code, in two passes:
    #
    # First pass:
    # %COMPILEDDEFS%  The compiled type definition.
    #
    # Second pass:
    # %TYPE%          The fully qualified type name.
    # %IVARDECS%      Instance variable declarations
    # %TVARDECS%      Type variable declarations
    # %TCONSTBODY%    Type constructor body
    # %INSTANCEVARS%  The compiled instance variable initialization code.
    # %TYPEVARS%      The compiled type variable initialization code.

    # This is the overall type template.
    variable typeTemplate

    # This is the normal type proc
    variable nominalTypeProc

    # This is the "-hastypemethods no" type proc
    variable simpleTypeProc
}

set ::snit::typeTemplate {

    #-------------------------------------------------------------------
    # The type's namespace definition and the user's type variables

    namespace eval %TYPE% {%TYPEVARS%
    }

    #----------------------------------------------------------------
    # Commands for use in methods, typemethods, etc.
    #
    # These are implemented as aliases into the Snit runtime library.

    interp alias {} %TYPE%::installhull  {} ::snit::RT.installhull %TYPE%
    interp alias {} %TYPE%::install      {} ::snit::RT.install %TYPE%
    interp alias {} %TYPE%::typevariable {} ::variable
    interp alias {} %TYPE%::variable     {} ::snit::RT.variable
    interp alias {} %TYPE%::mytypevar    {} ::snit::RT.mytypevar %TYPE%
    interp alias {} %TYPE%::typevarname  {} ::snit::RT.mytypevar %TYPE%
    interp alias {} %TYPE%::myvar        {} ::snit::RT.myvar
    interp alias {} %TYPE%::varname      {} ::snit::RT.myvar
    interp alias {} %TYPE%::codename     {} ::snit::RT.codename %TYPE%
    interp alias {} %TYPE%::myproc       {} ::snit::RT.myproc %TYPE%
    interp alias {} %TYPE%::mymethod     {} ::snit::RT.mymethod 
    interp alias {} %TYPE%::mytypemethod {} ::snit::RT.mytypemethod %TYPE%
    interp alias {} %TYPE%::from         {} ::snit::RT.from %TYPE%

    #-------------------------------------------------------------------
    # Snit's internal variables

    namespace eval %TYPE% {
        # Array: General Snit Info
        #
        # ns:                The type's namespace
        # hasinstances:      T or F, from pragma -hasinstances.
        # simpledispatch:    T or F, from pragma -hasinstances.
        # canreplace:        T or F, from pragma -canreplace.
        # counter:           Count of instances created so far.
        # widgetclass:       Set by widgetclass statement.
        # hulltype:          Hull type (frame or toplevel) for widgets only.
        # exceptmethods:     Methods explicitly not delegated to *
        # excepttypemethods: Methods explicitly not delegated to *
        # tvardecs:          Type variable declarations--for dynamic methods
        # ivardecs:          Instance variable declarations--for dyn. methods
        typevariable Snit_info
        set Snit_info(ns)      %TYPE%::
        set Snit_info(hasinstances) 1
        set Snit_info(simpledispatch) 0
        set Snit_info(canreplace) 0
        set Snit_info(counter) 0
        set Snit_info(widgetclass) {}
        set Snit_info(hulltype) frame
        set Snit_info(exceptmethods) {}
        set Snit_info(excepttypemethods) {}
        set Snit_info(tvardecs) {%TVARDECS%}
        set Snit_info(ivardecs) {%IVARDECS%}

        # Array: Public methods of this type.
        # The index is the method name, or "*".
        # The value is [list $pattern $componentName], where
        # $componentName is "" for normal methods.
        typevariable Snit_typemethodInfo
        array unset Snit_typemethodInfo

        # Array: Public methods of instances of this type.
        # The index is the method name, or "*".
        # The value is [list $pattern $componentName], where
        # $componentName is "" for normal methods.
        typevariable Snit_methodInfo
        array unset Snit_methodInfo

        # Array: option information.  See dictionary.txt.
        typevariable Snit_optionInfo
        array unset Snit_optionInfo
        set Snit_optionInfo(local)     {}
        set Snit_optionInfo(delegated) {}
        set Snit_optionInfo(starcomp)  {}
        set Snit_optionInfo(except)    {}
    }

    #----------------------------------------------------------------
    # Compiled Procs
    #
    # These commands are created or replaced during compilation:


    # Snit_instanceVars selfns
    #
    # Initializes the instance variables, if any.  Called during
    # instance creation.
    
    proc %TYPE%::Snit_instanceVars {selfns} {
        %INSTANCEVARS%
    }

    # Type Constructor
    proc %TYPE%::Snit_typeconstructor {type} {
        %TVARDECS%
        %TCONSTBODY%
    }

    #----------------------------------------------------------------
    # Default Procs
    #
    # These commands might be replaced during compilation:

    # Snit_destructor type selfns win self
    #
    # Default destructor for the type.  By default, it does
    # nothing.  It's replaced by any user destructor.
    # For types, it's called by method destroy; for widgettypes,
    # it's called by a destroy event handler.

    proc %TYPE%::Snit_destructor {type selfns win self} { }

    #----------------------------------------------------------
    # Compiled Definitions

    %COMPILEDDEFS%

    #----------------------------------------------------------
    # Finally, call the Type Constructor

    %TYPE%::Snit_typeconstructor %TYPE%
}

#-----------------------------------------------------------------------
# Type procs
#
# These procs expect the fully-qualified type name to be 
# substituted in for %TYPE%.

# This is the nominal type proc.  It supports typemethods and
# delegated typemethods.
set ::snit::nominalTypeProc {
    # Type dispatcher function.  Note: This function lives
    # in the parent of the %TYPE% namespace!  All accesses to 
    # %TYPE% variables and methods must be qualified!
    proc %TYPE% {{method ""} args} {
        # First, if there's no method, and no args, and there's a create
        # method, and this isn't a widget, then method is "create" and 
        # "args" is %AUTO%.
        if {$method eq "" && [llength $args] == 0} {
            ::variable %TYPE%::Snit_info

            if {$Snit_info(hasinstances) && !$Snit_info(isWidget)} {
                set method create
                lappend args %AUTO%
            } else {
                error "wrong \# args: should be \"%TYPE% method args\""
            }
        }

        # Next, retrieve the command.
	variable %TYPE%::Snit_typemethodCache
        while 1 {
            if {[catch {set Snit_typemethodCache($method)} commandRec]} {
                set commandRec [::snit::RT.CacheTypemethodCommand %TYPE% $method]

                if {[llength $commandRec] == 0} {
                    return -code error  "\"%TYPE% $method\" is not defined"
                }
            }

            # If we've got a real command, break.
            if {[lindex $commandRec 0] == 0} {
                break
            }

            # Otherwise, we need to look up again...if we can.
            if {[llength $args] == 0} {
                return -code error                   "wrong number args: should be \"%TYPE% $method method args\""
            }

            lappend method [lindex $args 0]
            set args [lrange $args 1 end]
        }

        set command [lindex $commandRec 1]

        # Pass along the return code unchanged.
        set retval [catch {uplevel 1 $command $args} result]

        if {$retval} {
            if {$retval == 1} {
                global errorInfo
                global errorCode
                return -code error -errorinfo $errorInfo                      -errorcode $errorCode $result
            } else {
                return -code $retval $result
            }
        }

        return $result
    }
}

# This is the simplified type proc for when there are no typemethods
# except create.  In this case, it doesn't take a method argument;
# the method is always "create".
set ::snit::simpleTypeProc {
    # Type dispatcher function.  Note: This function lives
    # in the parent of the %TYPE% namespace!  All accesses to 
    # %TYPE% variables and methods must be qualified!
    proc %TYPE% {args} {
        ::variable %TYPE%::Snit_info

        # FIRST, if the are no args, the single arg is %AUTO%
        if {[llength $args] == 0} {
            if {$Snit_info(isWidget)} {
                error "wrong \# args: should be \"%TYPE% name args\""
            }
            
            lappend args %AUTO%
        }

        # NEXT, we're going to call the create method.
        # Pass along the return code unchanged.
        if {$Snit_info(isWidget)} {
            set command [list ::snit::RT.widget.typemethod.create %TYPE%]
        } else {
            set command [list ::snit::RT.type.typemethod.create %TYPE%]
        }

        set retval [catch {uplevel 1 $command $args} result]

        if {$retval} {
            if {$retval == 1} {
                global errorInfo
                global errorCode
                return -code error -errorinfo $errorInfo                      -errorcode $errorCode $result
            } else {
                return -code $retval $result
            }
        }

        return $result
    }
}

#-----------------------------------------------------------------------
# Instance procs
#
# The following must be substituted into these proc bodies:
#
# %SELFNS%       The instance namespace
# %WIN%          The original instance name
# %TYPE%         The fully-qualified type name
#

# Nominal instance proc body: supports method caching and delegation.
#
# proc $instanceName {method args} ....
set ::snit::nominalInstanceProc {
    set self [set %SELFNS%::Snit_instance]

    while {1} {
        if {[catch {set %SELFNS%::Snit_methodCache($method)} commandRec]} {
            set commandRec [snit::RT.CacheMethodCommand %TYPE% %SELFNS% %WIN% $self $method]
                
            if {[llength $commandRec] == 0} {
                return -code error                      "\"$self $method\" is not defined"
            }
        }

        # If we've got a real command, break.
        if {[lindex $commandRec 0] == 0} {
            break
        }

        # Otherwise, we need to look up again...if we can.
        if {[llength $args] == 0} {
            return -code error                  "wrong number args: should be \"$self $method method args\""
        }

        lappend method [lindex $args 0]
        set args [lrange $args 1 end]
    }

    set command [lindex $commandRec 1]

    # Pass along the return code unchanged.
    set retval [catch {uplevel 1 $command $args} result]

    if {$retval} {
        if {$retval == 1} {
            global errorInfo
            global errorCode
            return -code error -errorinfo $errorInfo                  -errorcode $errorCode $result
        } else {
            return -code $retval $result
        }
    }

    return $result
}

# Simplified method proc body: No delegation allowed; no support for
# upvar or exotic return codes or hierarchical methods.  Designed for 
# max speed for simple types.
#
# proc $instanceName {method args} ....

set ::snit::simpleInstanceProc {
    set self [set %SELFNS%::Snit_instance]

    if {[lsearch -exact ${%TYPE%::Snit_methods} $method] == -1} {
	set optlist [join ${%TYPE%::Snit_methods} ", "]
	set optlist [linsert $optlist "end-1" "or"]
	error "bad option \"$method\": must be $optlist"
    }

    eval [linsert $args 0                %TYPE%::Snit_method$method %TYPE% %SELFNS% %WIN% $self] 
}


#=======================================================================
# Snit Type Definition
#
# These are the procs used to define Snit types, widgets, and 
# widgetadaptors.


#-----------------------------------------------------------------------
# Snit Compilation Variables
#
# The following variables are used while Snit is compiling a type,
# and are disposed afterwards.

namespace eval ::snit:: {
    # The compiler variable contains the name of the slave interpreter
    # used to compile type definitions.
    variable compiler ""

    # The compile array accumulates information about the type or
    # widgettype being compiled.  It is cleared before and after each
    # compilation.  It has these indices:
    #
    # type:                  The name of the type being compiled, for use
    #                        in compilation procs.
    # defs:                  Compiled definitions, both standard and client.
    # which:                 type, widget, widgetadaptor
    # instancevars:          Instance variable definitions and initializations.
    # ivprocdec:             Instance variable proc declarations.
    # tvprocdec:             Type variable proc declarations.
    # typeconstructor:       Type constructor body.
    # widgetclass:           The widgetclass, for snit::widgets, only
    # hasoptions:            False, initially; set to true when first
    #                        option is defined.
    # localoptions:          Names of local options.
    # delegatedoptions:      Names of delegated options.
    # localmethods:          Names of locally defined methods.
    # delegatesmethods:      no if no delegated methods, yes otherwise.
    # hashierarchic       :  no if no hierarchic methods, yes otherwise.
    # components:            Names of defined components.
    # typecomponents:        Names of defined typecomponents.
    # typevars:              Typevariable definitions and initializations.
    # varnames:              Names of instance variables
    # typevarnames           Names of type variables
    # hasconstructor         False, initially; true when constructor is
    #                        defined.
    # resource-$opt          The option's resource name
    # class-$opt             The option's class
    # -default-$opt          The option's default value
    # -validatemethod-$opt   The option's validate method
    # -configuremethod-$opt  The option's configure method
    # -cgetmethod-$opt       The option's cget method.
    # -hastypeinfo           The -hastypeinfo pragma
    # -hastypedestroy        The -hastypedestroy pragma
    # -hastypemethods        The -hastypemethods pragma
    # -hasinfo               The -hasinfo pragma
    # -hasinstances          The -hasinstances pragma
    # -simpledispatch        The -simpledispatch pragma
    # -canreplace            The -canreplace pragma
    variable compile

    # This variable accumulates method dispatch information; it has
    # the same structure as the %TYPE%::Snit_methodInfo array, and is
    # used to initialize it.
    variable methodInfo

    # This variable accumulates typemethod dispatch information; it has
    # the same structure as the %TYPE%::Snit_typemethodInfo array, and is
    # used to initialize it.
    variable typemethodInfo

    # The following variable lists the reserved type definition statement
    # names, e.g., the names you can't use as macros.  It's built at
    # compiler definition time using "info commands".
    variable reservedwords {}
}

#-----------------------------------------------------------------------
# type compilation commands
#
# The type and widgettype commands use a slave interpreter to compile
# the type definition.  These are the procs
# that are aliased into it.

# Initialize the compiler
proc ::snit::Comp.Init {} {
    variable compiler
    variable reservedwords

    if {$compiler eq ""} {
        # Create the compiler's interpreter
        set compiler [interp create]

        # Initialize the interpreter
	$compiler eval {
            # Load package information
            # TBD: see if this can be moved outside.
            catch {package require ::snit::__does_not_exist__}

            # Protect some Tcl commands our type definitions
            # will shadow.
            rename proc _proc
            rename variable _variable
        }

        # Define compilation aliases.
        $compiler alias pragma          ::snit::Comp.statement.pragma
        $compiler alias widgetclass     ::snit::Comp.statement.widgetclass
        $compiler alias hulltype        ::snit::Comp.statement.hulltype
        $compiler alias constructor     ::snit::Comp.statement.constructor
        $compiler alias destructor      ::snit::Comp.statement.destructor
        $compiler alias option          ::snit::Comp.statement.option
        $compiler alias oncget          ::snit::Comp.statement.oncget
        $compiler alias onconfigure     ::snit::Comp.statement.onconfigure
        $compiler alias method          ::snit::Comp.statement.method
        $compiler alias typemethod      ::snit::Comp.statement.typemethod
        $compiler alias typeconstructor ::snit::Comp.statement.typeconstructor
        $compiler alias proc            ::snit::Comp.statement.proc
        $compiler alias typevariable    ::snit::Comp.statement.typevariable
        $compiler alias variable        ::snit::Comp.statement.variable
        $compiler alias typecomponent   ::snit::Comp.statement.typecomponent
        $compiler alias component       ::snit::Comp.statement.component
        $compiler alias delegate        ::snit::Comp.statement.delegate
        $compiler alias expose          ::snit::Comp.statement.expose

        # Get the list of reserved words
        set reservedwords [$compiler eval {info commands}]
    }
}

# Compile a type definition, and return the results as a list of two
# items: the fully-qualified type name, and a script that will define
# the type when executed.
#
# which		type, widget, or widgetadaptor
# type          the type name
# body          the type definition
proc ::snit::Comp.Compile {which type body} {
    variable typeTemplate
    variable nominalTypeProc
    variable simpleTypeProc
    variable compile
    variable compiler
    variable methodInfo
    variable typemethodInfo

    # FIRST, qualify the name.
    if {![string match "::*" $type]} {
        # Get caller's namespace; 
        # append :: if not global namespace.
        set ns [uplevel 2 [list namespace current]]
        if {"::" != $ns} {
            append ns "::"
        }
        
        set type "$ns$type"
    }

    # NEXT, create and initialize the compiler, if needed.
    Comp.Init

    # NEXT, initialize the class data
    array unset methodInfo
    array unset typemethodInfo

    array unset compile
    set compile(type) $type
    set compile(defs) {}
    set compile(which) $which
    set compile(hasoptions) no
    set compile(localoptions) {}
    set compile(instancevars) {}
    set compile(typevars) {}
    set compile(delegatedoptions) {}
    set compile(ivprocdec) {}
    set compile(tvprocdec) {}
    set compile(typeconstructor) {}
    set compile(widgetclass) {}
    set compile(hulltype) {}
    set compile(localmethods) {}
    set compile(delegatesmethods) no
    set compile(hashierarchic) no
    set compile(components) {}
    set compile(typecomponents) {}
    set compile(varnames) {}
    set compile(typevarnames) {}
    set compile(hasconstructor) no
    set compile(-hastypedestroy) yes
    set compile(-hastypeinfo) yes
    set compile(-hastypemethods) yes
    set compile(-hasinfo) yes
    set compile(-hasinstances) yes
    set compile(-simpledispatch) no
    set compile(-canreplace) no

    set isWidget [string match widget* $which]
    set isWidgetAdaptor [string match widgetadaptor $which]

    # NEXT, Evaluate the type's definition in the class interpreter.
    $compiler eval $body

    # NEXT, Add the standard definitions
    append compile(defs)          "\nset %TYPE%::Snit_info(isWidget) $isWidget\n"

    append compile(defs)          "\nset %TYPE%::Snit_info(isWidgetAdaptor) $isWidgetAdaptor\n"

    # Indicate whether the type can create instances that replace
    # existing commands.
    append compile(defs) "\nset %TYPE%::Snit_info(canreplace) $compile(-canreplace)\n"


    # Check pragmas for conflict.
    
    if {!$compile(-hastypemethods) && !$compile(-hasinstances)} {
        error "$which $type has neither typemethods nor instances"
    }

    if {$compile(-simpledispatch) && $compile(delegatesmethods)} {
        error "$which $type requests -simpledispatch but delegates methods."
    }

    if {$compile(-simpledispatch) && $compile(hashierarchic)} {
        error "$which $type requests -simpledispatch but defines hierarchical methods."
    }

    # If there are typemethods, define the standard typemethods and
    # the nominal type proc.  Otherwise define the simple type proc.
    if {$compile(-hastypemethods)} {
        # Add the info typemethod unless the pragma forbids it.
        if {$compile(-hastypeinfo)} {
            Comp.statement.delegate typemethod info                  using {::snit::RT.typemethod.info %t}
        }

        # Add the destroy typemethod unless the pragma forbids it.
        if {$compile(-hastypedestroy)} {
            Comp.statement.delegate typemethod destroy                  using {::snit::RT.typemethod.destroy %t}
        }

        # Add the nominal type proc.
        append compile(defs) $nominalTypeProc
    } else {
        # Add the simple type proc.
        append compile(defs) $simpleTypeProc
    }

    # Add standard methods/typemethods that only make sense if the
    # type has instances.
    if {$compile(-hasinstances)} {
        # If we're using simple dispatch, remember that.
        if {$compile(-simpledispatch)} {
            append compile(defs) "\nset %TYPE%::Snit_info(simpledispatch) 1\n"
        }

        # Add the info method unless the pragma forbids it.
        if {$compile(-hasinfo)} {
            if {!$compile(-simpledispatch)} {
                Comp.statement.delegate method info                      using {::snit::RT.method.info %t %n %w %s}
            } else {
                Comp.statement.method info {args} {
                    eval [linsert $args 0                                ::snit::RT.method.info $type $selfns $win $self]
                }
            }
        }
        
        # Add the option handling stuff if there are any options.
        if {$compile(hasoptions)} {
            Comp.statement.variable options

            if {!$compile(-simpledispatch)} {
                Comp.statement.delegate method cget                      using {::snit::RT.method.cget %t %n %w %s}
                Comp.statement.delegate method configurelist                      using {::snit::RT.method.configurelist %t %n %w %s}
                Comp.statement.delegate method configure                      using {::snit::RT.method.configure %t %n %w %s}
            } else {
                Comp.statement.method cget {args} {
                    eval [linsert $args 0                                ::snit::RT.method.cget $type $selfns $win $self]
                }
                Comp.statement.method configurelist {args} {
                    eval [linsert $args 0                                ::snit::RT.method.configurelist $type $selfns $win $self]
                }
                Comp.statement.method configure {args} {
                    eval [linsert $args 0                                ::snit::RT.method.configure $type $selfns $win $self]
                }
            }
        }

        # Add a default constructor, if they haven't already defined one.
        # If there are options, it will configure args; otherwise it
        # will do nothing.
        if {!$compile(hasconstructor)} {
            if {$compile(hasoptions)} {
                Comp.statement.constructor {args} {
                    $self configurelist $args
                }
            } else {
                Comp.statement.constructor {} {}
            }
        }
        
        if {!$isWidget} {
            if {!$compile(-simpledispatch)} {
                Comp.statement.delegate method destroy                      using {::snit::RT.method.destroy %t %n %w %s}
            } else {
                Comp.statement.method destroy {args} {
                    eval [linsert $args 0                                ::snit::RT.method.destroy $type $selfns $win $self]
                }
            }

            Comp.statement.delegate typemethod create                  using {::snit::RT.type.typemethod.create %t}
        } else {
            Comp.statement.delegate typemethod create                  using {::snit::RT.widget.typemethod.create %t}
        }

        # Save the list of method names, for -simpledispatch; otherwise,
        # save the method info. 
        if {$compile(-simpledispatch)} {
            append compile(defs)                  "\nset %TYPE%::Snit_methods [list $compile(localmethods)]\n"
        } else {
            append compile(defs)                  "\narray set %TYPE%::Snit_methodInfo [list [array get methodInfo]]\n"
        }

    } else {
        append compile(defs) "\nset %TYPE%::Snit_info(hasinstances) 0\n"
    }

    # NEXT, compiling the type definition built up a set of information
    # about the type's locally defined options; add this information to
    # the compiled definition.
    Comp.SaveOptionInfo

    # NEXT, compiling the type definition built up a set of information
    # about the typemethods; save the typemethod info.
    append compile(defs)          "\narray set %TYPE%::Snit_typemethodInfo [list [array get typemethodInfo]]\n"

    # NEXT, if this is a widget define the hull component if it isn't
    # already defined.
    if {$isWidget} {
        Comp.DefineComponent hull
    }

    # NEXT, substitute the compiled definition into the type template
    # to get the type definition script.
    set defscript [Expand $typeTemplate                         %COMPILEDDEFS% $compile(defs)]

    # NEXT, substitute the defined macros into the type definition script.
    # This is done as a separate step so that the compile(defs) can 
    # contain the macros defined below.

    set defscript [Expand $defscript                         %TYPE%         $type                         %IVARDECS%     $compile(ivprocdec)                         %TVARDECS%     $compile(tvprocdec)                         %TCONSTBODY%   $compile(typeconstructor)                         %INSTANCEVARS% $compile(instancevars)                         %TYPEVARS%     $compile(typevars)  		       ]

    array unset compile

    return [list $type $defscript]
}

# Information about locally-defined options is accumulated during
# compilation, but not added to the compiled definition--the option
# statement can appear multiple times, so it's easier this way.
# This proc fills in Snit_optionInfo with the accumulated information.
#
# It also computes the option's resource and class names if needed.
#
# Note that the information for delegated options was put in 
# Snit_optionInfo during compilation.

proc ::snit::Comp.SaveOptionInfo {} {
    variable compile

    foreach option $compile(localoptions) {
        if {$compile(resource-$option) eq ""} {
            set compile(resource-$option) [string range $option 1 end]
        }

        if {$compile(class-$option) eq ""} {
            set compile(class-$option) [Capitalize $compile(resource-$option)]
        }

        # NOTE: Don't verify that the validate, configure, and cget 
        # values name real methods; the methods might be defined outside 
        # the typedefinition using snit::method.
        
        Mappend compile(defs) {
            # Option %OPTION%
            lappend %TYPE%::Snit_optionInfo(local) %OPTION%

            set %TYPE%::Snit_optionInfo(islocal-%OPTION%)   1
            set %TYPE%::Snit_optionInfo(resource-%OPTION%)  %RESOURCE%
            set %TYPE%::Snit_optionInfo(class-%OPTION%)     %CLASS%
            set %TYPE%::Snit_optionInfo(default-%OPTION%)   %DEFAULT%
            set %TYPE%::Snit_optionInfo(validate-%OPTION%)  %VALIDATE%
            set %TYPE%::Snit_optionInfo(configure-%OPTION%) %CONFIGURE%
            set %TYPE%::Snit_optionInfo(cget-%OPTION%)      %CGET%
            set %TYPE%::Snit_optionInfo(readonly-%OPTION%)  %READONLY%
        }   %OPTION%    $option              %RESOURCE%  $compile(resource-$option)              %CLASS%     $compile(class-$option)              %DEFAULT%   [list $compile(-default-$option)]              %VALIDATE%  [list $compile(-validatemethod-$option)]              %CONFIGURE% [list $compile(-configuremethod-$option)]              %CGET%      [list $compile(-cgetmethod-$option)]              %READONLY%  $compile(-readonly-$option)
    }
}


# Evaluates a compiled type definition, thus making the type available.
proc ::snit::Comp.Define {compResult} {
    # The compilation result is a list containing the fully qualified
    # type name and a script to evaluate to define the type.
    set type [lindex $compResult 0]
    set defscript [lindex $compResult 1]

    # Execute the type definition script.
    # Consider using namespace eval %TYPE%.  See if it's faster.
    if {[catch {eval $defscript} result]} {
        namespace delete $type
        catch {rename $type ""}
        error $result
    }

    return $type
}

# Sets pragma options which control how the type is defined.
proc ::snit::Comp.statement.pragma {args} {
    variable compile

    set errRoot "Error in \"pragma...\""

    foreach {opt val} $args {
        switch -exact -- $opt {
            -hastypeinfo    -
            -hastypedestroy -
            -hastypemethods -
            -hasinstances   -
            -simpledispatch -
            -hasinfo        -
            -canreplace     {
                if {![string is boolean -strict $val]} {
                    error "$errRoot, \"$opt\" requires a boolean value"
                }
                set compile($opt) $val
            }
            default {
                error "$errRoot, unknown pragma"
            }
        }
    }
}

# Defines a widget's option class name.  
# This statement is only available for snit::widgets,
# not for snit::types or snit::widgetadaptors.
proc ::snit::Comp.statement.widgetclass {name} {
    variable compile

    # First, widgetclass can only be set for true widgets
    if {"widget" != $compile(which)} {
        error "widgetclass cannot be set for snit::$compile(which)s"
    }

    # Next, validate the option name.  We'll require that it begin
    # with an uppercase letter.
    set initial [string index $name 0]
    if {![string is upper $initial]} {
        error "widgetclass \"$name\" does not begin with an uppercase letter"
    }

    if {"" != $compile(widgetclass)} {
        error "too many widgetclass statements"
    }

    # Next, save it.
    Mappend compile(defs) {
        set  %TYPE%::Snit_info(widgetclass) %WIDGETCLASS%
    } %WIDGETCLASS% [list $name]

    set compile(widgetclass) $name
}

# Defines a widget's hull type.
# This statement is only available for snit::widgets,
# not for snit::types or snit::widgetadaptors.
proc ::snit::Comp.statement.hulltype {name} {
    variable compile

    # First, hulltype can only be set for true widgets
    if {"widget" != $compile(which)} {
        error "hulltype cannot be set for snit::$compile(which)s"
    }

    # Next, it must be either "frame" or "toplevel"
    if {"frame" != $name && "toplevel" != $name} {
        error "invalid hulltype \"$name\", should be \"frame\" or \"toplevel\""
    }

    if {"" != $compile(hulltype)} {
        error "too many hulltype statements"
    }

    # Next, save it.
    Mappend compile(defs) {
        set  %TYPE%::Snit_info(hulltype) %HULLTYPE%
    } %HULLTYPE% $name

    set compile(hulltype) $name
}

# Defines a constructor.
proc ::snit::Comp.statement.constructor {arglist body} {
    variable compile

    CheckArgs "constructor" $arglist

    # Next, add a magic reference to self.
    set arglist [concat type selfns win self $arglist]

    # Next, add variable declarations to body:
    set body "%TVARDECS%%IVARDECS%\n$body"

    set compile(hasconstructor) yes
    append compile(defs) "proc %TYPE%::Snit_constructor [list $arglist] [list $body]\n"
} 

# Defines a destructor.
proc ::snit::Comp.statement.destructor {body} {
    variable compile

    # Next, add variable declarations to body:
    set body "%TVARDECS%%IVARDECS%\n$body"

    append compile(defs) "proc %TYPE%::Snit_destructor {type selfns win self} [list $body]\n\n"
} 

# Defines a type option.  The option value can be a triple, specifying
# the option's -name, resource name, and class name. 
proc ::snit::Comp.statement.option {optionDef args} {
    variable compile

    # First, get the three option names.
    set option [lindex $optionDef 0]
    set resourceName [lindex $optionDef 1]
    set className [lindex $optionDef 2]

    set errRoot "Error in \"option [list $optionDef]...\""

    # Next, validate the option name.
    if {![Comp.OptionNameIsValid $option]} {
        error "$errRoot, badly named option \"$option\""
    }

    if {[Contains $option $compile(delegatedoptions)]} {
        error "$errRoot, cannot define \"$option\" locally, it has been delegated"
    }

    if {![Contains $option $compile(localoptions)]} {
        # Remember that we've seen this one.
        set compile(hasoptions) yes
        lappend compile(localoptions) $option
        
        # Initialize compilation info for this option.
        set compile(resource-$option)         ""
        set compile(class-$option)            ""
        set compile(-default-$option)         ""
        set compile(-validatemethod-$option)  ""
        set compile(-configuremethod-$option) ""
        set compile(-cgetmethod-$option)      ""
        set compile(-readonly-$option)        0
    }

    # NEXT, see if we have a resource name.  If so, make sure it
    # isn't being redefined differently.
    if {$resourceName ne ""} {
        if {$compile(resource-$option) eq ""} {
            # If it's undefined, just save the value.
            set compile(resource-$option) $resourceName
        } elseif {$resourceName ne $compile(resource-$option)} {
            # It's been redefined differently.
            error "$errRoot, resource name redefined from \"$compile(resource-$option)\" to \"$resourceName\""
        }
    }

    # NEXT, see if we have a class name.  If so, make sure it
    # isn't being redefined differently.
    if {$className ne ""} {
        if {$compile(class-$option) eq ""} {
            # If it's undefined, just save the value.
            set compile(class-$option) $className
        } elseif {$className ne $compile(class-$option)} {
            # It's been redefined differently.
            error "$errRoot, class name redefined from \"$compile(class-$option)\" to \"$className\""
        }
    }

    # NEXT, handle the args; it's not an error to redefine these.
    if {[llength $args] == 1} {
        set compile(-default-$option) [lindex $args 0]
    } else {
        foreach {optopt val} $args {
            switch -exact -- $optopt {
                -default         -
                -validatemethod  -
                -configuremethod -
                -cgetmethod      {
                    set compile($optopt-$option) $val
                }
                -readonly        {
                    if {![string is boolean -strict $val]} {
                        error "$errRoot, -readonly requires a boolean, got \"$val\""
                    }
                    set compile($optopt-$option) $val
                }
                default {
                    error "$errRoot, unknown option definition option \"$optopt\""
                }
            }
        }
    }
}

# 1 if the option name is valid, 0 otherwise.
proc ::snit::Comp.OptionNameIsValid {option} {
    if {![string match {-*} $option] || [string match {*[A-Z ]*} $option]} {
        return 0
    }

    return 1
}

# Defines an option's cget handler
proc ::snit::Comp.statement.oncget {option body} {
    variable compile

    set errRoot "Error in \"oncget $option...\""

    if {[lsearch -exact $compile(delegatedoptions) $option] != -1} {
        return -code error "$errRoot, option \"$option\" is delegated"
    }

    if {[lsearch -exact $compile(localoptions) $option] == -1} {
        return -code error "$errRoot, option \"$option\" unknown"
    }

    # Next, add variable declarations to body:
    set body "%TVARDECS%%IVARDECS%\n$body"

    Comp.statement.method _cget$option {_option} $body
    Comp.statement.option $option -cgetmethod _cget$option
} 

# Defines an option's configure handler.
proc ::snit::Comp.statement.onconfigure {option arglist body} {
    variable compile

    if {[lsearch -exact $compile(delegatedoptions) $option] != -1} {
        return -code error "onconfigure $option: option \"$option\" is delegated"
    }

    if {[lsearch -exact $compile(localoptions) $option] == -1} {
        return -code error "onconfigure $option: option \"$option\" unknown"
    }

    if {[llength $arglist] != 1} {
        error         "onconfigure $option handler should have one argument, got \"$arglist\""
    }

    CheckArgs "onconfigure $option" $arglist

    # Next, add a magic reference to the option name
    set arglist [concat _option $arglist]

    Comp.statement.method _configure$option $arglist $body
    Comp.statement.option $option -configuremethod _configure$option
} 

# Defines an instance method.
proc ::snit::Comp.statement.method {method arglist body} {
    variable compile
    variable methodInfo

    # FIRST, check the method name against previously defined 
    # methods.
    Comp.CheckMethodName $method 0 ::snit::methodInfo          "Error in \"method [list $method]...\""

    if {[llength $method] > 1} {
        set compile(hashierarchic) yes
    }

    # Remeber this method
    lappend compile(localmethods) $method

    CheckArgs "method [list $method]" $arglist

    # Next, add magic references to type and self.
    set arglist [concat type selfns win self $arglist]

    # Next, add variable declarations to body:
    set body "%TVARDECS%%IVARDECS%\n$body"

    # Next, save the definition script.
    if {[llength $method] == 1} {
        set methodInfo($method) {0 "%t::Snit_method%m %t %n %w %s" ""}
        Mappend compile(defs) {
            proc %TYPE%::Snit_method%METHOD% %ARGLIST% %BODY% 
        } %METHOD% $method %ARGLIST% [list $arglist] %BODY% [list $body] 
    } else {
        set methodInfo($method) {0 "%t::Snit_hmethod%j %t %n %w %s" ""}

        Mappend compile(defs) {
            proc %TYPE%::Snit_hmethod%JMETHOD% %ARGLIST% %BODY% 
        } %JMETHOD% [join $method _] %ARGLIST% [list $arglist]              %BODY% [list $body] 
    }
} 

# Check for name collisions; save prefix information.
#
# method	The name of the method or typemethod.
# delFlag       1 if delegated, 0 otherwise.
# infoVar       The fully qualified name of the array containing 
#               information about the defined methods.
# errRoot       The root string for any error messages.

proc ::snit::Comp.CheckMethodName {method delFlag infoVar errRoot} {
    upvar $infoVar methodInfo

    # FIRST, make sure the method name is a valid Tcl list.
    if {[catch {lindex $method 0}]} {
        error "$errRoot, the name \"$method\" must have list syntax."
    }

    # NEXT, check whether we can define it.
    if {![catch {set methodInfo($method)} data]} {
        # We can't redefine methods with submethods.
        if {[lindex $data 0] == 1} {
            error "$errRoot, \"$method\" has submethods."
        }
       
        # You can't delegate a method that's defined locally,
        # and you can't define a method locally if it's been delegated.
        if {$delFlag && [lindex $data 2] eq ""} {
            error "$errRoot, \"$method\" has been defined locally."
        } elseif {!$delFlag && [lindex $data 2] ne ""} {
            error "$errRoot, \"$method\" has been delegated"
        }
    }

    # Handle hierarchical case.
    if {[llength $method] > 1} {
        set prefix {}
        set tokens $method
        while {[llength $tokens] > 1} {
            lappend prefix [lindex $tokens 0]
            set tokens [lrange $tokens 1 end]

            if {![catch {set methodInfo($prefix)} result]} {
                # Prefix is known.  If it's not a prefix, throw an
                # error.
                if {[lindex $result 0] == 0} {
                    error "$errRoot, \"$prefix\" has no submethods."
                }
            }
            
            set methodInfo($prefix) [list 1]
        }
    }
}

# Defines a typemethod method.
proc ::snit::Comp.statement.typemethod {method arglist body} {
    variable compile
    variable typemethodInfo

    # FIRST, check the typemethod name against previously defined 
    # typemethods.
    Comp.CheckMethodName $method 0 ::snit::typemethodInfo          "Error in \"typemethod [list $method]...\""

    CheckArgs "typemethod $method" $arglist

    # First, add magic reference to type.
    set arglist [concat type $arglist]

    # Next, add typevariable declarations to body:
    set body "%TVARDECS%\n$body"

    # Next, save the definition script
    if {[llength $method] == 1} {
        set typemethodInfo($method) {0 "%t::Snit_typemethod%m %t" ""}

        Mappend compile(defs) {
            proc %TYPE%::Snit_typemethod%METHOD% %ARGLIST% %BODY%
        } %METHOD% $method %ARGLIST% [list $arglist] %BODY% [list $body]
    } else {
        set typemethodInfo($method) {0 "%t::Snit_htypemethod%j %t" ""}

        Mappend compile(defs) {
            proc %TYPE%::Snit_htypemethod%JMETHOD% %ARGLIST% %BODY%
        } %JMETHOD% [join $method _]              %ARGLIST% [list $arglist] %BODY% [list $body]
    }
} 


# Defines a type constructor.
proc ::snit::Comp.statement.typeconstructor {body} {
    variable compile

    if {"" != $compile(typeconstructor)} {
        error "too many typeconstructors"
    }

    set compile(typeconstructor) $body
} 

# Defines a static proc in the type's namespace.
proc ::snit::Comp.statement.proc {proc arglist body} {
    variable compile

    # If "ns" is defined, the proc can see instance variables.
    if {[lsearch -exact $arglist selfns] != -1} {
        # Next, add instance variable declarations to body:
        set body "%IVARDECS%\n$body"
    }

    # The proc can always see typevariables.
    set body "%TVARDECS%\n$body"

    append compile(defs) "

        # Proc $proc
        proc [list %TYPE%::$proc $arglist $body]
    "
} 

# Defines a static variable in the type's namespace.
proc ::snit::Comp.statement.typevariable {name args} {
    variable compile

    set errRoot "Error in \"typevariable $name...\""

    set len [llength $args]
    
    if {$len > 2 ||
        ($len == 2 && [lindex $args 0] ne "-array")} {
        error "$errRoot, too many initializers"
    }

    if {[lsearch -exact $compile(varnames) $name] != -1} {
        error "$errRoot, \"$name\" is already an instance variable"
    }

    lappend compile(typevarnames) $name

    if {$len == 1} {
        append compile(typevars)  		"\n\t    [list ::variable $name [lindex $args 0]]"
    } elseif {$len == 2} {
        append compile(typevars)              "\n\t    [list ::variable $name]"
        append compile(typevars)              "\n\t    [list array set $name [lindex $args 1]]"
    } else {
        append compile(typevars)  		"\n\t    [list ::variable $name]"
    }

    append compile(tvprocdec) "\n\t    typevariable ${name}"
} 

# Defines an instance variable; the definition will go in the
# type's create typemethod.
proc ::snit::Comp.statement.variable {name args} {
    variable compile

    set errRoot "Error in \"variable $name...\""

    set len [llength $args]
    
    if {$len > 2 ||
        ($len == 2 && [lindex $args 0] ne "-array")} {
        error "$errRoot, too many initializers"
    }

    if {[lsearch -exact $compile(typevarnames) $name] != -1} {
        error "$errRoot, \"$name\" is already a typevariable"
    }

    lappend compile(varnames) $name

    if {$len == 1} {
        append compile(instancevars)              "\nset \${selfns}::$name [list [lindex $args 0]]\n"
    } elseif {$len == 2} {
        append compile(instancevars)              "\narray set \${selfns}::$name [list [lindex $args 1]]\n"
    } 

    append  compile(ivprocdec) "\n\t    "
    Mappend compile(ivprocdec) {::variable ${selfns}::%N} %N $name 
} 

# Defines a typecomponent, and handles component options.
#
# component     The logical name of the delegate
# args          options.

proc ::snit::Comp.statement.typecomponent {component args} {
    variable compile

    set errRoot "Error in \"typecomponent $component...\""

    # FIRST, define the component
    Comp.DefineTypecomponent $component $errRoot

    # NEXT, handle the options.
    set publicMethod ""
    set inheritFlag 0

    foreach {opt val} $args {
        switch -exact -- $opt {
            -public {
                set publicMethod $val
            }
            -inherit {
                set inheritFlag $val
                if {![string is boolean $inheritFlag]} {
    error "typecomponent $component -inherit: expected boolean value, got \"$val\""
                }
            }
            default {
                error "typecomponent $component: Invalid option \"$opt\""
            }
        }
    }

    # NEXT, if -public specified, define the method.  
    if {$publicMethod ne ""} {
        Comp.statement.delegate typemethod [list $publicMethod *] to $component
    }

    # NEXT, if "-inherit 1" is specified, delegate typemethod * to 
    # this component.
    if {$inheritFlag} {
        Comp.statement.delegate typemethod "*" to $component
    }

}


# Defines a name to be a typecomponent
# 
# The name becomes a typevariable; in addition, it gets a 
# write trace so that when it is set, all of the component mechanisms
# get updated.
#
# component     The component name

proc ::snit::Comp.DefineTypecomponent {component {errRoot "Error"}} {
    variable compile

    if {[lsearch -exact $compile(varnames) $component] != -1} {
        error "$errRoot, \"$component\" is already an instance variable"
    }

    if {[lsearch -exact $compile(typecomponents) $component] == -1} {
        # Remember we've done this.
        lappend compile(typecomponents) $component

        # Make it a type variable with no initial value
        Comp.statement.typevariable $component ""

        # Add a write trace to do the component thing.
        Mappend compile(typevars) {
            trace add variable %COMP% write                  [list ::snit::RT.TypecomponentTrace [list %TYPE%] %COMP%]
        } %TYPE% $compile(type) %COMP% $component
    }
} 

# Defines a component, and handles component options.
#
# component     The logical name of the delegate
# args          options.
#
# TBD: Ideally, it should be possible to call this statement multiple
# times, possibly changing the option values.  To do that, I'd need
# to cache the option values and not act on them until *after* I'd
# read the entire type definition.

proc ::snit::Comp.statement.component {component args} {
    variable compile

    set errRoot "Error in \"component $component...\""

    # FIRST, define the component
    Comp.DefineComponent $component $errRoot

    # NEXT, handle the options.
    set publicMethod ""
    set inheritFlag 0

    foreach {opt val} $args {
        switch -exact -- $opt {
            -public {
                set publicMethod $val
            }
            -inherit {
                set inheritFlag $val
                if {![string is boolean $inheritFlag]} {
    error "component $component -inherit: expected boolean value, got \"$val\""
                }
            }
            default {
                error "component $component: Invalid option \"$opt\""
            }
        }
    }

    # NEXT, if -public specified, define the method.  
    if {$publicMethod ne ""} {
        Comp.statement.delegate method [list $publicMethod *] to $component
    }

    # NEXT, if -inherit is specified, delegate method/option * to 
    # this component.
    if {$inheritFlag} {
        Comp.statement.delegate method "*" to $component
        Comp.statement.delegate option "*" to $component
    }
}


# Defines a name to be a component
# 
# The name becomes an instance variable; in addition, it gets a 
# write trace so that when it is set, all of the component mechanisms
# get updated.
#
# component     The component name

proc ::snit::Comp.DefineComponent {component {errRoot "Error"}} {
    variable compile

    if {[lsearch -exact $compile(typevarnames) $component] != -1} {
        error "$errRoot, \"$component\" is already a typevariable"
    }

    if {[lsearch -exact $compile(components) $component] == -1} {
        # Remember we've done this.
        lappend compile(components) $component

        # Make it an instance variable with no initial value
        Comp.statement.variable $component ""

        # Add a write trace to do the component thing.
        Mappend compile(instancevars) {
            trace add variable ${selfns}::%COMP% write                  [list ::snit::RT.ComponentTrace [list %TYPE%] $selfns %COMP%]
        } %TYPE% $compile(type) %COMP% $component
    }
} 

# Creates a delegated method, typemethod, or option.
proc ::snit::Comp.statement.delegate {what name args} {
    # FIRST, dispatch to correct handler.
    switch $what {
        typemethod { Comp.DelegatedTypemethod $name $args }
        method     { Comp.DelegatedMethod     $name $args }
        option     { Comp.DelegatedOption     $name $args }
        default {
            error "Error in \"delegate $what $name...\", \"$what\"?"
        }
    }

    if {([llength $args] % 2) != 0} {
        error "Error in \"delegate $what $name...\", invalid syntax"
    }
}

# Creates a delegated typemethod delegating it to a particular
# typecomponent or an arbitrary command.
#
# method    The name of the method
# arglist       Delegation options

proc ::snit::Comp.DelegatedTypemethod {method arglist} {
    variable compile
    variable typemethodInfo

    set errRoot "Error in \"delegate typemethod [list $method]...\""

    # Next, parse the delegation options.
    set component ""
    set target ""
    set exceptions {}
    set pattern ""
    set methodTail [lindex $method end]

    foreach {opt value} $arglist {
        switch -exact $opt {
            to     { set component $value  }
            as     { set target $value     }
            except { set exceptions $value }
            using  { set pattern $value    }
            default {
                error "$errRoot, unknown delegation option \"$opt\""
            }
        }
    }

    if {$component eq "" && $pattern eq ""} {
        error "$errRoot, missing \"to\""
    }

    if {$methodTail eq "*" && $target ne ""} {
        error "$errRoot, cannot specify \"as\" with \"*\""
    }

    if {$methodTail ne "*" && $exceptions ne ""} {
        error "$errRoot, can only specify \"except\" with \"*\"" 
    }

    if {$pattern ne "" && $target ne ""} {
        error "$errRoot, cannot specify both \"as\" and \"using\""
    }

    foreach token [lrange $method 1 end-1] {
        if {$token eq "*"} {
            error "$errRoot, \"*\" must be the last token."
        }
    }

    # NEXT, define the component
    if {$component ne ""} {
        Comp.DefineTypecomponent $component $errRoot
    }

    # NEXT, define the pattern.
    if {$pattern eq ""} {
        if {$methodTail eq "*"} {
            set pattern "%c %m"
        } elseif {$target ne ""} {
            set pattern "%c $target"
        } else {
            set pattern "%c %m"
        }
    }

    # Make sure the pattern is a valid list.
    if {[catch {lindex $pattern 0} result]} {
        error "$errRoot, the using pattern, \"$pattern\", is not a valid list"
    }

    # NEXT, check the method name against previously defined 
    # methods.
    Comp.CheckMethodName $method 1 ::snit::typemethodInfo $errRoot

    set typemethodInfo($method) [list 0 $pattern $component]

    if {[string equal $methodTail "*"]} {
        Mappend compile(defs) {
            set %TYPE%::Snit_info(excepttypemethods) %EXCEPT%
        } %EXCEPT% [list $exceptions]
    }
}


# Creates a delegated method delegating it to a particular
# component or command.
#
# method        The name of the method
# arglist       Delegation options.

proc ::snit::Comp.DelegatedMethod {method arglist} {
    variable compile
    variable methodInfo

    set errRoot "Error in \"delegate method [list $method]...\""

    # Next, parse the delegation options.
    set component ""
    set target ""
    set exceptions {}
    set pattern ""
    set methodTail [lindex $method end]

    foreach {opt value} $arglist {
        switch -exact $opt {
            to     { set component $value  }
            as     { set target $value     }
            except { set exceptions $value }
            using  { set pattern $value    }
            default {
                error "$errRoot, unknown delegation option \"$opt\""
            }
        }
    }

    if {$component eq "" && $pattern eq ""} {
        error "$errRoot, missing \"to\""
    }

    if {$methodTail eq "*" && $target ne ""} {
        error "$errRoot, cannot specify \"as\" with \"*\""
    }

    if {$methodTail ne "*" && $exceptions ne ""} {
        error "$errRoot, can only specify \"except\" with \"*\"" 
    }

    if {$pattern ne "" && $target ne ""} {
        error "$errRoot, cannot specify both \"as\" and \"using\""
    }

    foreach token [lrange $method 1 end-1] {
        if {$token eq "*"} {
            error "$errRoot, \"*\" must be the last token."
        }
    }

    # NEXT, we delegate some methods
    set compile(delegatesmethods) yes

    # NEXT, define the component.  Allow typecomponents.
    if {$component ne ""} {
        if {[lsearch -exact $compile(typecomponents) $component] == -1} {
            Comp.DefineComponent $component $errRoot
        }
    }

    # NEXT, define the pattern.
    if {$pattern eq ""} {
        if {$methodTail eq "*"} {
            set pattern "%c %m"
        } elseif {$target ne ""} {
            set pattern "%c $target"
        } else {
            set pattern "%c %m"
        }
    }

    # Make sure the pattern is a valid list.
    if {[catch {lindex $pattern 0} result]} {
        error "$errRoot, the using pattern, \"$pattern\", is not a valid list"
    }

    # NEXT, check the method name against previously defined 
    # methods.
    Comp.CheckMethodName $method 1 ::snit::methodInfo $errRoot

    # NEXT, save the method info.
    set methodInfo($method) [list 0 $pattern $component]

    if {[string equal $methodTail "*"]} {
        Mappend compile(defs) {
            set %TYPE%::Snit_info(exceptmethods) %EXCEPT%
        } %EXCEPT% [list $exceptions]
    }
} 

# Creates a delegated option, delegating it to a particular
# component and, optionally, to a particular option of that
# component.
#
# optionDef     The option definition
# args          definition arguments.

proc ::snit::Comp.DelegatedOption {optionDef arglist} {
    variable compile

    # First, get the three option names.
    set option [lindex $optionDef 0]
    set resourceName [lindex $optionDef 1]
    set className [lindex $optionDef 2]

    set errRoot "Error in \"delegate option [list $optionDef]...\""

    # Next, parse the delegation options.
    set component ""
    set target ""
    set exceptions {}

    foreach {opt value} $arglist {
        switch -exact $opt {
            to     { set component $value  }
            as     { set target $value     }
            except { set exceptions $value }
            default {
                error "$errRoot, unknown delegation option \"$opt\""
            }
        }
    }

    if {$component eq ""} {
        error "$errRoot, missing \"to\""
    }

    if {$option eq "*" && $target ne ""} {
        error "$errRoot, cannot specify \"as\" with \"delegate option *\""
    }

    if {$option ne "*" && $exceptions ne ""} {
        error "$errRoot, can only specify \"except\" with \"delegate option *\"" 
    }

    # Next, validate the option name

    if {"*" != $option} {
        if {![Comp.OptionNameIsValid $option]} {
            error "$errRoot, badly named option \"$option\""
        }
    }

    if {[Contains $option $compile(localoptions)]} {
        error "$errRoot, \"$option\" has been defined locally"
    }

    if {[Contains $option $compile(delegatedoptions)]} {
        error "$errRoot, \"$option\" is multiply delegated"
    }

    # NEXT, define the component
    Comp.DefineComponent $component $errRoot

    # Next, define the target option, if not specified.
    if {![string equal $option "*"] &&
        [string equal $target ""]} {
        set target $option
    }

    # NEXT, save the delegation data.
    set compile(hasoptions) yes

    if {![string equal $option "*"]} {
        lappend compile(delegatedoptions) $option

        # Next, compute the resource and class names, if they aren't
        # already defined.

        if {"" == $resourceName} {
            set resourceName [string range $option 1 end]
        }

        if {"" == $className} {
            set className [Capitalize $resourceName]
        }

        Mappend  compile(defs) {
            set %TYPE%::Snit_optionInfo(islocal-%OPTION%) 0
            set %TYPE%::Snit_optionInfo(resource-%OPTION%) %RES%
            set %TYPE%::Snit_optionInfo(class-%OPTION%) %CLASS%
            lappend %TYPE%::Snit_optionInfo(delegated) %OPTION%
            set %TYPE%::Snit_optionInfo(target-%OPTION%) [list %COMP% %TARGET%]
            lappend %TYPE%::Snit_optionInfo(delegated-%COMP%) %OPTION%
        }   %OPTION% $option              %COMP% $component              %TARGET% $target              %RES% $resourceName              %CLASS% $className 
    } else {
        Mappend  compile(defs) {
            set %TYPE%::Snit_optionInfo(starcomp) %COMP%
            set %TYPE%::Snit_optionInfo(except) %EXCEPT%
        } %COMP% $component %EXCEPT% [list $exceptions]
    }
} 

# Exposes a component, effectively making the component's command an
# instance method.
#
# component     The logical name of the delegate
# "as"          sugar; if not "", must be "as"
# methodname    The desired method name for the component's command, or ""

proc ::snit::Comp.statement.expose {component {"as" ""} {methodname ""}} {
    variable compile


    # FIRST, define the component
    Comp.DefineComponent $component

    # NEXT, define the method just as though it were in the type
    # definition.
    if {[string equal $methodname ""]} {
        set methodname $component
    }

    Comp.statement.method $methodname args [Expand {
        if {[llength $args] == 0} {
            return $%COMPONENT%
        }

        if {[string equal $%COMPONENT% ""]} {
            error "undefined component \"%COMPONENT%\""
        }


        set cmd [linsert $args 0 $%COMPONENT%]
        return [uplevel 1 $cmd]
    } %COMPONENT% $component]
}



#-----------------------------------------------------------------------
# Public commands

# Compile a type definition, and return the results as a list of two
# items: the fully-qualified type name, and a script that will define
# the type when executed.
#
# which		type, widget, or widgetadaptor
# type          the type name
# body          the type definition
proc ::snit::compile {which type body} {
    return [Comp.Compile $which $type $body]
}

proc ::snit::type {type body} {
    return [Comp.Define [Comp.Compile type $type $body]]
}

proc ::snit::widget {type body} {
    return [Comp.Define [Comp.Compile widget $type $body]]
}

proc ::snit::widgetadaptor {type body} {
    return [Comp.Define [Comp.Compile widgetadaptor $type $body]]
}

proc ::snit::typemethod {type method arglist body} {
    # Make sure the type exists.
    if {![info exists ${type}::Snit_info]} {
        error "no such type: \"$type\""
    }

    upvar ${type}::Snit_info           Snit_info
    upvar ${type}::Snit_typemethodInfo Snit_typemethodInfo

    # FIRST, check the typemethod name against previously defined 
    # typemethods.
    Comp.CheckMethodName $method 0 ${type}::Snit_typemethodInfo          "Cannot define \"$method\""

    # NEXT, check the arguments
    CheckArgs "snit::typemethod $type $method" $arglist

    # Next, add magic reference to type.
    set arglist [concat type $arglist]

    # Next, add typevariable declarations to body:
    set body "$Snit_info(tvardecs)\n$body"

    # Next, define it.
    if {[llength $method] == 1} {
        set Snit_typemethodInfo($method) {0 "%t::Snit_typemethod%m %t" ""}
        uplevel 1 [list proc ${type}::Snit_typemethod$method $arglist $body]
    } else {
        set Snit_typemethodInfo($method) {0 "%t::Snit_htypemethod%j %t" ""}
        set suffix [join $method _]
        uplevel 1 [list proc ${type}::Snit_htypemethod$suffix $arglist $body]
    }
}

proc ::snit::method {type method arglist body} {
    # Make sure the type exists.
    if {![info exists ${type}::Snit_info]} {
        error "no such type: \"$type\""
    }

    upvar ${type}::Snit_methodInfo  Snit_methodInfo
    upvar ${type}::Snit_info        Snit_info

    # FIRST, check the method name against previously defined 
    # methods.
    Comp.CheckMethodName $method 0 ${type}::Snit_methodInfo          "Cannot define \"$method\""

    # NEXT, check the arguments
    CheckArgs "snit::method $type $method" $arglist

    # Next, add magic references to type and self.
    set arglist [concat type selfns win self $arglist]

    # Next, add variable declarations to body:
    set body "$Snit_info(tvardecs)$Snit_info(ivardecs)\n$body"

    # Next, define it.
    if {[llength $method] == 1} {
        set Snit_methodInfo($method) {0 "%t::Snit_method%m %t %n %w %s" ""}
        uplevel 1 [list proc ${type}::Snit_method$method $arglist $body]
    } else {
        set Snit_methodInfo($method) {0 "%t::Snit_hmethod%j %t %n %w %s" ""}

        set suffix [join $method _]
        uplevel 1 [list proc ${type}::Snit_hmethod$suffix $arglist $body]
    }
}

# Defines a proc within the compiler; this proc can call other
# type definition statements, and thus can be used for meta-programming.
proc ::snit::macro {name arglist body} {
    variable compiler
    variable reservedwords

    # FIRST, make sure the compiler is defined.
    Comp.Init

    # NEXT, check the macro name against the reserved words
    if {[lsearch -exact $reservedwords $name] != -1} {
        error "invalid macro name \"$name\""
    }

    # NEXT, see if the name has a namespace; if it does, define the
    # namespace.
    set ns [namespace qualifiers $name]

    if {$ns ne ""} {
        $compiler eval "namespace eval $ns {}"
    }

    # NEXT, define the macro
    $compiler eval [list _proc $name $arglist $body]
}

#-----------------------------------------------------------------------
# Utility Functions
#
# These are utility functions used while compiling Snit types.

# Builds a template from a tagged list of text blocks, then substitutes
# all symbols in the mapTable, returning the expanded template.
proc ::snit::Expand {template args} {
    return [string map $args $template]
}

# Expands a template and appends it to a variable.
proc ::snit::Mappend {varname template args} {
    upvar $varname myvar

    append myvar [string map $args $template]
}

# Checks argument list against reserved args 
proc ::snit::CheckArgs {which arglist} {
    variable reservedArgs
    
    foreach name $reservedArgs {
        if {[Contains $name $arglist]} {
            error "$which's arglist may not contain \"$name\" explicitly"
        }
    }
}

# Returns 1 if a value is in a list, and 0 otherwise.
proc ::snit::Contains {value list} {
    if {[lsearch -exact $list $value] != -1} {
        return 1
    } else {
        return 0
    }
}

# Capitalizes the first letter of a string.
proc ::snit::Capitalize {text} {
    set first [string index $text 0]
    set rest [string range $text 1 end]
    return "[string toupper $first]$rest"
}

# Converts an arbitrary white-space-delimited string into a list
# by splitting on white-space and deleting empty tokens.

proc ::snit::Listify {str} {
    set result {}
    foreach token [split [string trim $str]] {
        if {[string length $token] > 0} {
            lappend result $token
        }
    }

    return $result
}


#=======================================================================
# Snit Runtime Library
#
# These are procs used by Snit types and widgets at runtime.

#-----------------------------------------------------------------------
# Object Creation

# Creates a new instance of the snit::type given its name and the args.
#
# type		The snit::type
# name		The instance name
# args		Args to pass to the constructor

proc ::snit::RT.type.typemethod.create {type name args} {
    variable ${type}::Snit_info
    variable ${type}::Snit_optionInfo

    # FIRST, qualify the name.
    if {![string match "::*" $name]} {
        # Get caller's namespace; 
        # append :: if not global namespace.
        set ns [uplevel 1 [list namespace current]]
        if {"::" != $ns} {
            append ns "::"
        }
        
        set name "$ns$name"
    }

    # NEXT, if %AUTO% appears in the name, generate a unique 
    # command name.  Otherwise, ensure that the name isn't in use.
    if {[string match "*%AUTO%*" $name]} {
        set name [::snit::RT.UniqueName Snit_info(counter) $type $name]
    } elseif {!$Snit_info(canreplace) && [info commands $name] ne ""} {
        error "command \"$name\" already exists"
    }

    # NEXT, create the instance's namespace.
    set selfns          [::snit::RT.UniqueInstanceNamespace Snit_info(counter) $type]
    namespace eval $selfns {}

    # NEXT, install the dispatcher
    RT.MakeInstanceCommand $type $selfns $name

    # Initialize the options to their defaults. 
    upvar ${selfns}::options options
    foreach opt $Snit_optionInfo(local) {
        set options($opt) $Snit_optionInfo(default-$opt)
    }
        
    # Initialize the instance vars to their defaults.
    # selfns must be defined, as it is used implicitly.
    ${type}::Snit_instanceVars $selfns

    # Execute the type's constructor.
    set errcode [catch {
        RT.ConstructInstance $type $selfns $name $args
    } result]

    if {$errcode} {
        global errorInfo
        global errorCode
        
        set theInfo $errorInfo
        set theCode $errorCode
        ::snit::RT.DestroyObject $type $selfns $name
        error "Error in constructor: $result" $theInfo $theCode
    }

    # NEXT, return the object's name.
    return $name
}

# Creates a new instance of the snit::widget or snit::widgetadaptor
# given its name and the args.
#
# type		The snit::widget or snit::widgetadaptor
# name		The instance name
# args		Args to pass to the constructor

proc ::snit::RT.widget.typemethod.create {type name args} {
    variable ${type}::Snit_info
    variable ${type}::Snit_optionInfo

    # FIRST, if %AUTO% appears in the name, generate a unique 
    # command name.
    if {[string match "*%AUTO%*" $name]} {
        set name [::snit::RT.UniqueName Snit_info(counter) $type $name]
    }
            
    # NEXT, create the instance's namespace.
    set selfns          [::snit::RT.UniqueInstanceNamespace Snit_info(counter) $type]
    namespace eval $selfns { }
            
    # NEXT, Initialize the widget's own options to their defaults.
    upvar ${selfns}::options options
    foreach opt $Snit_optionInfo(local) {
        set options($opt) $Snit_optionInfo(default-$opt)
    }

    # Initialize the instance vars to their defaults.
    ${type}::Snit_instanceVars $selfns

    # NEXT, if this is a normal widget (not a widget adaptor) then 
    # create a frame as its hull.  We set the frame's -class to
    # the user's widgetclass, or, if none, to the basename of
    # the $type with an initial upper case letter.
    if {!$Snit_info(isWidgetAdaptor)} {
        # FIRST, determine the class name
        if {"" == $Snit_info(widgetclass)} {
            set Snit_info(widgetclass)                  [::snit::Capitalize [namespace tail $type]]
        }

        # NEXT, create the widget
        set self $name
        package require Tk
        ${type}::installhull using              $Snit_info(hulltype) -class $Snit_info(widgetclass)

        # NEXT, let's query the option database for our
        # widget, now that we know that it exists.
        foreach opt $Snit_optionInfo(local) {
            set dbval [RT.OptionDbGet $type $name $opt]

            if {"" != $dbval} {
                set options($opt) $dbval
            }
        }
    }

    # Execute the type's constructor, and verify that it
    # has a hull.
    set errcode [catch {
        RT.ConstructInstance $type $selfns $name $args

        ::snit::RT.Component $type $selfns hull

        # Prepare to call the object's destructor when the
        # <Destroy> event is received.  Use a Snit-specific bindtag
        # so that the widget name's tag is unencumbered.

        bind Snit$type$name <Destroy> [::snit::Expand {
            ::snit::RT.DestroyObject %TYPE% %NS% %W
        } %TYPE% $type %NS% $selfns]

        # Insert the bindtag into the list of bindtags right
        # after the widget name.
        set taglist [bindtags $name]
        set ndx [lsearch -exact $taglist $name]
        incr ndx
        bindtags $name [linsert $taglist $ndx Snit$type$name]
    } result]

    if {$errcode} {
        global errorInfo
        global errorCode

        set theInfo $errorInfo
        set theCode $errorCode
        ::snit::RT.DestroyObject $type $selfns $name
        error "Error in constructor: $result" $theInfo $theCode
    }

    # NEXT, return the object's name.
    return $name
}


# RT.MakeInstanceCommand type selfns instance
#
# type        The object type
# selfns      The instance namespace
# instance    The instance name
#
# Creates the instance proc.

proc ::snit::RT.MakeInstanceCommand {type selfns instance} {
    variable ${type}::Snit_info
        
    # FIRST, remember the instance name.  The Snit_instance variable
    # allows the instance to figure out its current name given the
    # instance namespace.
    upvar ${selfns}::Snit_instance Snit_instance
    set Snit_instance $instance

    # NEXT, qualify the proc name if it's a widget.
    if {$Snit_info(isWidget)} {
        set procname ::$instance
    } else {
        set procname $instance
    }

    # NEXT, install the new proc
    if {!$Snit_info(simpledispatch)} {
        set instanceProc $::snit::nominalInstanceProc
    } else {
        set instanceProc $::snit::simpleInstanceProc
    }

    proc $procname {method args}          [string map               [list %SELFNS% $selfns %WIN% $instance %TYPE% $type]               $instanceProc]

    # NEXT, add the trace.
    trace add command $procname {rename delete}          [list ::snit::RT.InstanceTrace $type $selfns $instance]
}

# This proc is called when the instance command is renamed.
# If op is delete, then new will always be "", so op is redundant.
#
# type		The fully-qualified type name
# selfns	The instance namespace
# win		The original instance/tk window name.
# old		old instance command name
# new		new instance command name
# op		rename or delete
#
# If the op is delete, we need to clean up the object; otherwise,
# we need to track the change.
#
# NOTE: In Tcl 8.4.2 there's a bug: errors in rename and delete
# traces aren't propagated correctly.  Instead, they silently
# vanish.  Add a catch to output any error message.

proc ::snit::RT.InstanceTrace {type selfns win old new op} {
    variable ${type}::Snit_info

    # Note to developers ...
    # For Tcl 8.4.0, errors thrown in trace handlers vanish silently.
    # Therefore we catch them here and create some output to help in
    # debugging such problems.

    if {[catch {
        # FIRST, clean up if necessary
        if {"" == $new} {
            if {$Snit_info(isWidget)} {
                destroy $win
            } else {
                ::snit::RT.DestroyObject $type $selfns $win
            }
        } else {
            # Otherwise, track the change.
            variable ${selfns}::Snit_instance
            set Snit_instance [uplevel 1 [list namespace which -command $new]]
            
            # Also, clear the instance caches, as many cached commands
            # might be invalid.
            RT.ClearInstanceCaches $selfns
        }
    } result]} {
        global errorInfo
        # Pop up the console on Windows wish, to enable stdout.
        # This clobbers errorInfo on unix, so save it so we can print it.
        set ei $errorInfo
        catch {console show}
        puts "Error in ::snit::RT.InstanceTrace $type $selfns $win $old $new $op:"
        puts $ei
    }
}

# Calls the instance constructor and handles related housekeeping.
proc ::snit::RT.ConstructInstance {type selfns instance arglist} {
    variable ${type}::Snit_optionInfo
    variable ${selfns}::Snit_iinfo

    # Track whether we are constructed or not.
    set Snit_iinfo(constructed) 0

    # Call the user's constructor
    eval [linsert $arglist 0                ${type}::Snit_constructor $type $selfns $instance $instance]

    set Snit_iinfo(constructed) 1

    # Unset the configure cache for all -readonly options.
    # This ensures that the next time anyone tries to 
    # configure it, an error is thrown.
    foreach opt $Snit_optionInfo(local) {
        if {$Snit_optionInfo(readonly-$opt)} {
            unset -nocomplain ${selfns}::Snit_configureCache($opt)
        }
    }

    return
}

# Returns a unique command name.  
#
# REQUIRE: type is a fully qualified name.
# REQUIRE: name contains "%AUTO%"
# PROMISE: the returned command name is unused.
proc ::snit::RT.UniqueName {countervar type name} {
    upvar $countervar counter 
    while 1 {
        # FIRST, bump the counter and define the %AUTO% instance name;
        # then substitute it into the specified name.  Wrap around at
        # 2^31 - 2 to prevent overflow problems.
        incr counter
        if {$counter > 2147483646} {
            set counter 0
        }
        set auto "[namespace tail $type]$counter"
        set candidate [Expand $name %AUTO% $auto]
        if {[info commands $candidate] eq ""} {
            return $candidate
        }
    }
}

# Returns a unique instance namespace, fully qualified.
#
# countervar     The name of a counter variable
# type           The instance's type
#
# REQUIRE: type is fully qualified
# PROMISE: The returned namespace name is unused.

proc ::snit::RT.UniqueInstanceNamespace {countervar type} {
    upvar $countervar counter 
    while 1 {
        # FIRST, bump the counter and define the namespace name.
        # Then see if it already exists.  Wrap around at
        # 2^31 - 2 to prevent overflow problems.
        incr counter
        if {$counter > 2147483646} {
            set counter 0
        }
        set ins "${type}::Snit_inst${counter}"
        if {![namespace exists $ins]} {
            return $ins
        }
    }
}

# Retrieves an option's value from the option database.
# Returns "" if no value is found.
proc ::snit::RT.OptionDbGet {type self opt} {
    variable ${type}::Snit_optionInfo

    return [option get $self                  $Snit_optionInfo(resource-$opt)                  $Snit_optionInfo(class-$opt)]
}

#-----------------------------------------------------------------------
# Object Destruction

# Implements the standard "destroy" method
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name

proc ::snit::RT.method.destroy {type selfns win self} {
    # Calls Snit_cleanup, which (among other things) calls the
    # user's destructor.
    ::snit::RT.DestroyObject $type $selfns $win
}

# This is the function that really cleans up; it's automatically 
# called when any instance is destroyed, e.g., by "$object destroy"
# for types, and by the <Destroy> event for widgets.
#
# type		The fully-qualified type name.
# selfns	The instance namespace
# win		The original instance command name.

proc ::snit::RT.DestroyObject {type selfns win} {
    variable ${type}::Snit_info

    # If the variable Snit_instance doesn't exist then there's no
    # instance command for this object -- it's most likely a 
    # widgetadaptor. Consequently, there are some things that
    # we don't need to do.
    if {[info exists ${selfns}::Snit_instance]} {
        upvar ${selfns}::Snit_instance instance
            
        # First, remove the trace on the instance name, so that we
        # don't call RT.DestroyObject recursively.
        RT.RemoveInstanceTrace $type $selfns $win $instance
            
        # Next, call the user's destructor
        ${type}::Snit_destructor $type $selfns $win $instance

        # Next, if this isn't a widget, delete the instance command.
        # If it is a widget, get the hull component's name, and rename
        # it back to the widget name
                
        # Next, delete the hull component's instance command,
        # if there is one.
        if {$Snit_info(isWidget)} {
            set hullcmd [::snit::RT.Component $type $selfns hull]
            
            catch {rename $instance ""}

            # Clear the bind event
            bind Snit$type$win <Destroy> ""

            if {[info command $hullcmd] != ""} {
                # FIRST, rename the hull back to its original name.
                # If the hull is itself a megawidget, it will have its
                # own cleanup to do, and it might not do it properly
                # if it doesn't have the right name.
                rename $hullcmd ::$instance

                # NEXT, destroy it.
                destroy $instance
            }
        } else {
            catch {rename $instance ""}
        }
    }

    # Next, delete the instance's namespace.  This kills any
    # instance variables.
    namespace delete $selfns
}

# Remove instance trace
# 
# type           The fully qualified type name
# selfns         The instance namespace
# win            The original instance name/Tk window name
# instance       The current instance name

proc ::snit::RT.RemoveInstanceTrace {type selfns win instance} {
    variable ${type}::Snit_info

    if {$Snit_info(isWidget)} {
        set procname ::$instance
    } else {
        set procname $instance
    }
        
    # NEXT, remove any trace on this name
    catch {
        trace remove command $procname {rename delete}              [list ::snit::RT.InstanceTrace $type $selfns $win]
    }
}

#-----------------------------------------------------------------------
# Typecomponent Management and Method Caching

# Typecomponent trace; used for write trace on typecomponent 
# variables.  Saves the new component object name, provided 
# that certain conditions are met.  Also clears the typemethod
# cache.

proc ::snit::RT.TypecomponentTrace {type component n1 n2 op} {
    upvar ${type}::Snit_info Snit_info
    upvar ${type}::${component} cvar
    upvar ${type}::Snit_typecomponents Snit_typecomponents
        
    # Save the new component value.
    set Snit_typecomponents($component) $cvar

    # Clear the typemethod cache.
    # TBD: can we unset just the elements related to
    # this component?
    unset -nocomplain -- ${type}::Snit_typemethodCache
}

# Generates and caches the command for a typemethod.
#
# type		The type
# method	The name of the typemethod to call.
#
# The return value is one of the following lists:
#
#    {}              There's no such method.
#    {1}             The method has submethods; look again.
#    {0 <command>}   Here's the command to execute.

proc snit::RT.CacheTypemethodCommand {type method} {
    upvar ${type}::Snit_typemethodInfo  Snit_typemethodInfo
    upvar ${type}::Snit_typecomponents  Snit_typecomponents
    upvar ${type}::Snit_typemethodCache Snit_typemethodCache
    upvar ${type}::Snit_info            Snit_info
    
    # FIRST, get the pattern data and the typecomponent name.
    set implicitCreate 0
    set instanceName ""

    set starredMethod [lreplace $method end end *]
    set methodTail [lindex $method end]

    if {[info exists Snit_typemethodInfo($method)]} {
        set key $method
    } elseif {[info exists Snit_typemethodInfo($starredMethod)]} {
        if {[lsearch -exact $Snit_info(excepttypemethods) $methodTail] == -1} {
            set key $starredMethod
        } else {
            return [list ]
        }
    } elseif {$Snit_info(hasinstances)} {
        # Assume the unknown name is an instance name to create, unless
        # this is a widget and the style of the name is wrong, or the
        # name mimics a standard typemethod.

        if {[set ${type}::Snit_info(isWidget)] && 
            ![string match ".*" $method]} {
            return [list ]
        }

        # Without this check, the call "$type info" will redefine the
        # standard "::info" command, with disastrous results.  Since it's
        # a likely thing to do if !-typeinfo, put in an explicit check.
        if {$method eq "info" || $method eq "destroy"} {
            return [list ]
        }

        set implicitCreate 1
        set instanceName $method
        set key create
        set method create
    } else {
        return [list ]
    }
    
    foreach {flag pattern compName} $Snit_typemethodInfo($key) {}

    if {$flag == 1} {
        return [list 1]
    }

    # NEXT, build the substitution list
    set subList [list                       %% %                       %t $type                       %M $method                       %m [lindex $method end]                       %j [join $method _]]
    
    if {$compName ne ""} {
        if {![info exists Snit_typecomponents($compName)]} {
            error "$type delegates typemethod \"$method\" to undefined typecomponent \"$compName\""
        }
        
        lappend subList %c [list $Snit_typecomponents($compName)]
    }

    set command {}

    foreach subpattern $pattern {
        lappend command [string map $subList $subpattern]
    }

    if {$implicitCreate} {
        # In this case, $method is the name of the instance to
        # create.  Don't cache, as we usually won't do this one
        # again.
        lappend command $instanceName
    } else {
        set Snit_typemethodCache($method) [list 0 $command]
    }

    return [list 0 $command]
}


#-----------------------------------------------------------------------
# Component Management and Method Caching

# Retrieves the object name given the component name.
proc ::snit::RT.Component {type selfns name} {
    variable ${selfns}::Snit_components

    if {[catch {set Snit_components($name)} result]} {
        variable ${selfns}::Snit_instance

        error "component \"$name\" is undefined in $type $Snit_instance"
    }
    
    return $result
}

# Component trace; used for write trace on component instance 
# variables.  Saves the new component object name, provided 
# that certain conditions are met.  Also clears the method
# cache.

proc ::snit::RT.ComponentTrace {type selfns component n1 n2 op} {
    upvar ${type}::Snit_info Snit_info
    upvar ${selfns}::${component} cvar
    upvar ${selfns}::Snit_components Snit_components
        
    # If they try to redefine the hull component after
    # it's been defined, that's an error--but only if
    # this is a widget or widget adaptor.
    if {"hull" == $component && 
        $Snit_info(isWidget) &&
        [info exists Snit_components($component)]} {
        set cvar $Snit_components($component)
        error "The hull component cannot be redefined"
    }

    # Save the new component value.
    set Snit_components($component) $cvar

    # Clear the instance caches.
    # TBD: can we unset just the elements related to
    # this component?
    RT.ClearInstanceCaches $selfns
}

# Generates and caches the command for a method.
#
# type:		The instance's type
# selfns:	The instance's private namespace
# win:          The instance's original name (a Tk widget name, for
#               snit::widgets.
# self:         The instance's current name.
# method:	The name of the method to call.
#
# The return value is one of the following lists:
#
#    {}              There's no such method.
#    {1}             The method has submethods; look again.
#    {0 <command>}   Here's the command to execute.

proc ::snit::RT.CacheMethodCommand {type selfns win self method} {
    variable ${type}::Snit_info
    variable ${type}::Snit_methodInfo
    variable ${type}::Snit_typecomponents
    variable ${selfns}::Snit_components
    variable ${selfns}::Snit_methodCache

    # FIRST, get the pattern data and the component name.
    set starredMethod [lreplace $method end end *]
    set methodTail [lindex $method end]

    if {[info exists Snit_methodInfo($method)]} {
        set key $method
    } elseif {[info exists Snit_methodInfo($starredMethod)] &&
              [lsearch -exact $Snit_info(exceptmethods) $methodTail] == -1} {
        set key $starredMethod
    } else {
        return [list ]
    }

    foreach {flag pattern compName} $Snit_methodInfo($key) {}

    if {$flag == 1} {
        return [list 1]
    }

    # NEXT, build the substitution list
    set subList [list                       %% %                       %t $type                       %M $method                       %m [lindex $method end]                       %j [join $method _]                       %n [list $selfns]                       %w [list $win]                       %s [list $self]]

    if {$compName ne ""} {
        if {[info exists Snit_components($compName)]} {
            set compCmd $Snit_components($compName)
        } elseif {[info exists Snit_typecomponents($compName)]} {
            set compCmd $Snit_typecomponents($compName)
        } else {
            error "$type $self delegates method \"$method\" to undefined component \"$compName\""
        }
        
        lappend subList %c [list $compCmd]
    }

    # Note: The cached command will executed faster if it's
    # already a list.
    set command {}

    foreach subpattern $pattern {
        lappend command [string map $subList $subpattern]
    }

    set commandRec [list 0 $command]

    set Snit_methodCache($method) $commandRec
        
    return $commandRec
}


# Looks up a method's command.
#
# type:		The instance's type
# selfns:	The instance's private namespace
# win:          The instance's original name (a Tk widget name, for
#               snit::widgets.
# self:         The instance's current name.
# method:	The name of the method to call.
# errPrefix:    Prefix for any error method
proc ::snit::RT.LookupMethodCommand {type selfns win self method errPrefix} {
    set commandRec [snit::RT.CacheMethodCommand                          $type $selfns $win $self                          $method]


    if {[llength $commandRec] == 0} {
        return -code error              "$errPrefix, \"$self $method\" is not defined"
    } elseif {[lindex $commandRec 0] == 1} {
        return -code error              "$errPrefix, wrong number args: should be \"$self\" $method method args"
    }

    return  [lindex $commandRec 1]
}


# Clears all instance command caches
proc ::snit::RT.ClearInstanceCaches {selfns} {
    unset -nocomplain -- ${selfns}::Snit_methodCache
    unset -nocomplain -- ${selfns}::Snit_cgetCache
    unset -nocomplain -- ${selfns}::Snit_configureCache
    unset -nocomplain -- ${selfns}::Snit_validateCache
}


#-----------------------------------------------------------------------
# Component Installation

# Implements %TYPE%::installhull.  The variables self and selfns
# must be defined in the caller's context.
#
# Installs the named widget as the hull of a 
# widgetadaptor.  Once the widget is hijacked, its new name
# is assigned to the hull component.

proc ::snit::RT.installhull {type {using "using"} {widgetType ""} args} {
    variable ${type}::Snit_info
    variable ${type}::Snit_optionInfo
    upvar self self
    upvar selfns selfns
    upvar ${selfns}::hull hull
    upvar ${selfns}::options options

    # FIRST, make sure we can do it.
    if {!$Snit_info(isWidget)} { 
        error "installhull is valid only for snit::widgetadaptors"
    }
            
    if {[info exists ${selfns}::Snit_instance]} {
        error "hull already installed for $type $self"
    }

    # NEXT, has it been created yet?  If not, create it using
    # the specified arguments.
    if {"using" == $using} {
        # FIRST, create the widget
        set cmd [linsert $args 0 $widgetType $self]
        set obj [uplevel 1 $cmd]
            
        # NEXT, for each option explicitly delegated to the hull
        # that doesn't appear in the usedOpts list, get the
        # option database value and apply it--provided that the
        # real option name and the target option name are different.
        # (If they are the same, then the option database was
        # already queried as part of the normal widget creation.)
        #
        # Also, we don't need to worry about implicitly delegated
        # options, as the option and target option names must be
        # the same.
        if {[info exists Snit_optionInfo(delegated-hull)]} {
                
            # FIRST, extract all option names from args
            set usedOpts {}
            set ndx [lsearch -glob $args "-*"]
            foreach {opt val} [lrange $args $ndx end] {
                lappend usedOpts $opt
            }
                
            foreach opt $Snit_optionInfo(delegated-hull) {
                set target [lindex $Snit_optionInfo(target-$opt) 1]
                
                if {"$target" == $opt} {
                    continue
                }
                    
                set result [lsearch -exact $usedOpts $target]
                    
                if {$result != -1} {
                    continue
                }

                set dbval [RT.OptionDbGet $type $self $opt]
                $obj configure $target $dbval
            }
        }
    } else {
        set obj $using
        
        if {$obj ne $self} {
            error                  "hull name mismatch: \"$obj\" != \"$self\""
        }
    }

    # NEXT, get the local option defaults.
    foreach opt $Snit_optionInfo(local) {
        set dbval [RT.OptionDbGet $type $self $opt]
            
        if {"" != $dbval} {
            set options($opt) $dbval
        }
    }


    # NEXT, do the magic
    set i 0
    while 1 {
        incr i
        set newName "::hull${i}$self"
        if {"" == [info commands $newName]} {
            break
        }
    }
        
    rename ::$self $newName
    RT.MakeInstanceCommand $type $selfns $self
        
    # Note: this relies on RT.ComponentTrace to do the dirty work.
    set hull $newName
        
    return
}

# Implements %TYPE%::install.
#
# Creates a widget and installs it as the named component.
# It expects self and selfns to be defined in the caller's context.

proc ::snit::RT.install {type compName "using" widgetType winPath args} {
    variable ${type}::Snit_optionInfo
    variable ${type}::Snit_info
    upvar self self
    upvar selfns selfns
    upvar ${selfns}::$compName comp
    upvar ${selfns}::hull hull

    # We do the magic option database stuff only if $self is
    # a widget.
    if {$Snit_info(isWidget)} {
        if {"" == $hull} {
            error "tried to install \"$compName\" before the hull exists"
        }
            
        # FIRST, query the option database and save the results 
        # into args.  Insert them before the first option in the
        # list, in case there are any non-standard parameters.
        #
        # Note: there might not be any delegated options; if so,
        # don't bother.

        if {[info exists Snit_optionInfo(delegated-$compName)]} {
            set ndx [lsearch -glob $args "-*"]
                
            foreach opt $Snit_optionInfo(delegated-$compName) {
                set dbval [RT.OptionDbGet $type $self $opt]
                    
                if {"" != $dbval} {
                    set target [lindex $Snit_optionInfo(target-$opt) 1]
                    set args [linsert $args $ndx $target $dbval]
                }
            }
        }
    }
             
    # NEXT, create the component and save it.
    set cmd [concat [list $widgetType $winPath] $args]
    set comp [uplevel 1 $cmd]

    # NEXT, handle the option database for "delegate option *",
    # in widgets only.
    if {$Snit_info(isWidget) && $Snit_optionInfo(starcomp) eq $compName} {
        # FIRST, get the list of option specs from the widget.
        # If configure doesn't work, skip it.
        if {[catch {$comp configure} specs]} {
            return
        }

        # NEXT, get the set of explicitly used options from args
        set usedOpts {}
        set ndx [lsearch -glob $args "-*"]
        foreach {opt val} [lrange $args $ndx end] {
            lappend usedOpts $opt
        }

        # NEXT, "delegate option *" matches all options defined
        # by this widget that aren't defined by the widget as a whole,
        # and that aren't excepted.  Plus, we skip usedOpts.  So build 
        # a list of the options it can't match.
        set skiplist [concat                            $usedOpts                            $Snit_optionInfo(except)                            $Snit_optionInfo(local)                            $Snit_optionInfo(delegated)]
        
        # NEXT, loop over all of the component's options, and set
        # any not in the skip list for which there is an option 
        # database value.
        foreach spec $specs {
            # Skip aliases
            if {[llength $spec] != 5} {
                continue
            }

            set opt [lindex $spec 0]

            if {[lsearch -exact $skiplist $opt] != -1} {
                continue
            }

            set res [lindex $spec 1]
            set cls [lindex $spec 2]

            set dbvalue [option get $self $res $cls]

            if {"" != $dbvalue} {
                $comp configure $opt $dbvalue
            }
        }
    }

    return
}


#-----------------------------------------------------------------------
# Method/Variable Name Qualification

# Implements %TYPE%::variable.  Requires selfns.
proc ::snit::RT.variable {varname} {
    upvar selfns selfns

    if {![string match "::*" $varname]} {
        uplevel 1 [list upvar 1 ${selfns}::$varname $varname]
    } else {
        # varname is fully qualified; let the standard
        # "variable" command handle it.
        uplevel 1 [list ::variable $varname]
    }
}

# Fully qualifies a typevariable name.
#
# This is used to implement the mytypevar command.

proc ::snit::RT.mytypevar {type name} {
    return ${type}::$name
}

# Fully qualifies an instance variable name.
#
# This is used to implement the myvar command.
proc ::snit::RT.myvar {name} {
    upvar selfns selfns
    return ${selfns}::$name
}

# Use this like "list" to convert a proc call into a command
# string to pass to another object (e.g., as a -command).
# Qualifies the proc name properly.
#
# This is used to implement the "myproc" command.

proc ::snit::RT.myproc {type procname args} {
    set procname "${type}::$procname"
    return [linsert $args 0 $procname]
}

# DEPRECATED
proc ::snit::RT.codename {type name} {
    return "${type}::$name"
}

# Use this like "list" to convert a typemethod call into a command
# string to pass to another object (e.g., as a -command).
# Inserts the type command at the beginning.
#
# This is used to implement the "mytypemethod" command.

proc ::snit::RT.mytypemethod {type args} {
    return [linsert $args 0 $type]
}

# Use this like "list" to convert a method call into a command
# string to pass to another object (e.g., as a -command).
# Inserts the code at the beginning to call the right object, even if
# the object's name has changed.  Requires that selfns be defined
# in the calling context, eg. can only be called in instance
# code.
#
# This is used to implement the "mymethod" command.

proc ::snit::RT.mymethod {args} {
    upvar selfns selfns
    return [linsert $args 0 ::snit::RT.CallInstance ${selfns}]
}

# Calls an instance method for an object given its
# instance namespace and remaining arguments (the first of which
# will be the method name.
#
# selfns		The instance namespace
# args			The arguments
#
# Uses the selfns to determine $self, and calls the method
# in the normal way.
#
# This is used to implement the "mymethod" command.

proc ::snit::RT.CallInstance {selfns args} {
    upvar ${selfns}::Snit_instance self

    set retval [catch {uplevel 1 [linsert $args 0 $self]} result]

    if {$retval} {
        if {$retval == 1} {
            global errorInfo
            global errorCode
            return -code error -errorinfo $errorInfo                  -errorcode $errorCode $result
        } else {
            return -code $retval $result
        }
    }

    return $result
}

# Looks for the named option in the named variable.  If found,
# it and its value are removed from the list, and the value
# is returned.  Otherwise, the default value is returned.
# If the option is undelegated, it's own default value will be
# used if none is specified.
#
# Implements the "from" command.

proc ::snit::RT.from {type argvName option {defvalue ""}} {
    variable ${type}::Snit_optionInfo
    upvar $argvName argv

    set ioption [lsearch -exact $argv $option]

    if {$ioption == -1} {
        if {"" == $defvalue &&
            [info exists Snit_optionInfo(default-$option)]} {
            return $Snit_optionInfo(default-$option)
        } else {
            return $defvalue
        }
    }

    set ivalue [expr {$ioption + 1}]
    set value [lindex $argv $ivalue]
    
    set argv [lreplace $argv $ioption $ivalue] 

    return $value
}

#-----------------------------------------------------------------------
# Type Destruction

# Implements the standard "destroy" typemethod:
# Destroys a type completely.
#
# type		The snit type

proc ::snit::RT.typemethod.destroy {type} {
    variable ${type}::Snit_info
        
    # FIRST, destroy all instances
    foreach selfns [namespace children $type] {
        if {![namespace exists $selfns]} {
            continue
        }
        upvar ${selfns}::Snit_instance obj
            
        if {$Snit_info(isWidget)} {
            destroy $obj
        } else {
            if {"" != [info commands $obj]} {
                $obj destroy
            }
        }
    }

    # NEXT, destroy the type's data.
    namespace delete $type

    # NEXT, get rid of the type command.
    rename $type ""
}



#-----------------------------------------------------------------------
# Option Handling

# Implements the standard "cget" method
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# option        The name of the option

proc ::snit::RT.method.cget {type selfns win self option} {
    if {[catch {set ${selfns}::Snit_cgetCache($option)} command]} {
        set command [snit::RT.CacheCgetCommand $type $selfns $win $self $option]
        
        if {[llength $command] == 0} {
            return -code error "unknown option \"$option\""
        }
    }
            
    uplevel 1 $command
}

# Retrieves and caches the command that implements "cget" for the 
# specified option.
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# option        The name of the option

proc ::snit::RT.CacheCgetCommand {type selfns win self option} {
    variable ${type}::Snit_optionInfo
    variable ${selfns}::Snit_cgetCache
                
    if {[info exists Snit_optionInfo(islocal-$option)]} {
        # We know the item; it's either local, or explicitly delegated.
        if {$Snit_optionInfo(islocal-$option)} {
            # It's a local option.  If it has a cget method defined,
            # use it; otherwise just return the value.

            if {$Snit_optionInfo(cget-$option) eq ""} {
                set command [list set ${selfns}::options($option)]
            } else {
                set command [snit::RT.LookupMethodCommand                                   $type $selfns $win $self                                   $Snit_optionInfo(cget-$option)                                   "can't cget $option"]

                lappend command $option
            }

            set Snit_cgetCache($option) $command
            return $command
        }
         
        # Explicitly delegated option; get target
        set comp [lindex $Snit_optionInfo(target-$option) 0]
        set target [lindex $Snit_optionInfo(target-$option) 1]
    } elseif {$Snit_optionInfo(starcomp) ne "" &&
              [lsearch -exact $Snit_optionInfo(except) $option] == -1} {
        # Unknown option, but unknowns are delegated; get target.
        set comp $Snit_optionInfo(starcomp)
        set target $option
    } else {
        return ""
    }
    
    # Get the component's object.
    set obj [RT.Component $type $selfns $comp]

    set command [list $obj cget $target]
    set Snit_cgetCache($option) $command

    return $command
}

# Implements the standard "configurelist" method
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# optionlist    A list of options and their values.

proc ::snit::RT.method.configurelist {type selfns win self optionlist} {
    variable ${type}::Snit_optionInfo

    foreach {option value} $optionlist {
        # FIRST, get the configure command, caching it if need be.
        if {[catch {set ${selfns}::Snit_configureCache($option)} command]} {
            set command [snit::RT.CacheConfigureCommand                               $type $selfns $win $self $option]

            if {[llength $command] == 0} {
                return -code error "unknown option \"$option\""
            }
        }

        # NEXT, the caching the configure command also cached the
        # validate command, if any.  If we have one, run it.
        set valcommand [set ${selfns}::Snit_validateCache($option)]

        if {[llength $valcommand]} {
            lappend valcommand $value
            uplevel 1 $valcommand
        }

        # NEXT, configure the option with the value.
        lappend command $value
        uplevel 1 $command
    }
    
    return
}

# Retrieves and caches the command that stores the named option.
# Also stores the command that validates the name option if any;
# If none, the validate command is "", so that the cache is always
# populated.
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# option        An option name

proc ::snit::RT.CacheConfigureCommand {type selfns win self option} {
    variable ${type}::Snit_optionInfo
    variable ${selfns}::Snit_configureCache
    variable ${selfns}::Snit_validateCache

    if {[info exist Snit_optionInfo(islocal-$option)]} {
        # We know the item; it's either local, or explicitly delegated.
        
        if {$Snit_optionInfo(islocal-$option)} {
            # It's a local option.

            # If it's readonly, it throws an error if we're already 
            # constructed.
            if {$Snit_optionInfo(readonly-$option)} {
                if {[set ${selfns}::Snit_iinfo(constructed)]} {
                    error "option $option can only be set at instance creation"
                }
            }

            # If it has a validate method, cache that for later.
            if {$Snit_optionInfo(validate-$option) ne ""} {
                set command [snit::RT.LookupMethodCommand                                   $type $selfns $win $self                                   $Snit_optionInfo(validate-$option)                                   "can't validate $option"]

                lappend command $option
                set Snit_validateCache($option) $command
            } else {
                set Snit_validateCache($option) ""
            }
            
            # If it has a configure method defined,
            # cache it; otherwise, just set the value.

            if {$Snit_optionInfo(configure-$option) eq ""} {
                set command [list set ${selfns}::options($option)]
            } else {
                set command [snit::RT.LookupMethodCommand                                   $type $selfns $win $self                                   $Snit_optionInfo(configure-$option)                                   "can't configure $option"]

                lappend command $option
            }

            set Snit_configureCache($option) $command
            return $command
        }

        # Delegated option: get target.
        set comp [lindex $Snit_optionInfo(target-$option) 0]
        set target [lindex $Snit_optionInfo(target-$option) 1]
    } elseif {$Snit_optionInfo(starcomp) != "" &&
              [lsearch -exact $Snit_optionInfo(except) $option] == -1} {
        # Unknown option, but unknowns are delegated.
        set comp $Snit_optionInfo(starcomp)
        set target $option
    } else {
        return ""
    }

    # There is no validate command in this case; save an empty string.
    set Snit_validateCache($option) ""
        
    # Get the component's object
    set obj [RT.Component $type $selfns $comp]
    
    set command [list $obj configure $target]
    set Snit_configureCache($option) $command

    return $command
}

# Implements the standard "configure" method
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# args          A list of options and their values, possibly empty.

proc ::snit::RT.method.configure {type selfns win self args} {
    # If two or more arguments, set values as usual.
    if {[llength $args] >= 2} {
        ::snit::RT.method.configurelist $type $selfns $win $self $args
        return
    }

    # If zero arguments, acquire data for each known option
    # and return the list
    if {[llength $args] == 0} {
        set result {}
        foreach opt [RT.method.info.options $type $selfns $win $self] {
            # Refactor this, so that we don't need to call via $self.
            lappend result [RT.GetOptionDbSpec                                  $type $selfns $win $self $opt]
        }
        
        return $result
    }

    # They want it for just one.
    set opt [lindex $args 0]

    return [RT.GetOptionDbSpec $type $selfns $win $self $opt]
}


# Retrieves the option database spec for a single option.
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# option        The name of an option
#
# TBD: This is a bad name.  What it's returning is the
# result of the configure query.

proc ::snit::RT.GetOptionDbSpec {type selfns win self opt} {
    variable ${type}::Snit_optionInfo

    upvar ${selfns}::Snit_components Snit_components
    upvar ${selfns}::options         options
    
    if {[info exists options($opt)]} {
        # This is a locally-defined option.  Just build the
        # list and return it.
        set res $Snit_optionInfo(resource-$opt)
        set cls $Snit_optionInfo(class-$opt)
        set def $Snit_optionInfo(default-$opt)

        return [list $opt $res $cls $def                      [RT.method.cget $type $selfns $win $self $opt]]
    } elseif {[info exists Snit_optionInfo(target-$opt)]} {
        # This is an explicitly delegated option.  The only
        # thing we don't have is the default.
        set res $Snit_optionInfo(resource-$opt)
        set cls $Snit_optionInfo(class-$opt)
        
        # Get the default
        set logicalName [lindex $Snit_optionInfo(target-$opt) 0]
        set comp $Snit_components($logicalName)
        set target [lindex $Snit_optionInfo(target-$opt) 1]

        if {[catch {$comp configure $target} result]} {
            set defValue {}
        } else {
            set defValue [lindex $result 3]
        }

        return [list $opt $res $cls $defValue [$self cget $opt]]
    } elseif {$Snit_optionInfo(starcomp) ne "" &&
              [lsearch -exact $Snit_optionInfo(except) $opt] == -1} {
        set logicalName $Snit_optionInfo(starcomp)
        set target $opt
        set comp $Snit_components($logicalName)

        if {[catch {set value [$comp cget $target]} result]} {
            error "unknown option \"$opt\""
        }

        if {![catch {$comp configure $target} result]} {
            # Replace the delegated option name with the local name.
            return [::snit::Expand $result $target $opt]
        }

        # configure didn't work; return simple form.
        return [list $opt "" "" "" $value]
    } else {
        error "unknown option \"$opt\""
    }
}

#-----------------------------------------------------------------------
# Type Introspection

# Implements the standard "info" typemethod.
#
# type		The snit type
# command       The info subcommand
# args          All other arguments.

proc ::snit::RT.typemethod.info {type command args} {
    global errorInfo
    global errorCode

    switch -exact $command {
        typevars    -
        typemethods -
        instances {
            # TBD: it should be possible to delete this error
            # handling.
            set errflag [catch {
                uplevel 1 [linsert $args 0  			       ::snit::RT.typemethod.info.$command $type]
            } result]

            if {$errflag} {
                return -code error -errorinfo $errorInfo                      -errorcode $errorCode $result
            } else {
                return $result
            }
        }
        default {
            error "\"$type info $command\" is not defined"
        }
    }
}


# Returns a list of the type's typevariables whose names match a 
# pattern, excluding Snit internal variables.
#
# type		A Snit type
# pattern       Optional.  The glob pattern to match.  Defaults
#               to *.

proc ::snit::RT.typemethod.info.typevars {type {pattern *}} {
    set result {}
    foreach name [info vars "${type}::$pattern"] {
        set tail [namespace tail $name]
        if {![string match "Snit_*" $tail]} {
            lappend result $name
        }
    }
    
    return $result
}

# Returns a list of the type's methods whose names match a 
# pattern.  If "delegate typemethod *" is used, the list may
# not be complete.
#
# type		A Snit type
# pattern       Optional.  The glob pattern to match.  Defaults
#               to *.

proc ::snit::RT.typemethod.info.typemethods {type {pattern *}} {
    variable ${type}::Snit_typemethodInfo
    variable ${type}::Snit_typemethodCache

    # FIRST, get the explicit names, skipping prefixes.
    set result {}

    foreach name [array names Snit_typemethodInfo -glob $pattern] {
        if {[lindex $Snit_typemethodInfo($name) 0] != 1} {
            lappend result $name
        }
    }

    # NEXT, add any from the cache that aren't explicit.
    if {[info exists Snit_typemethodInfo(*)]} {
        # First, remove "*" from the list.
        set ndx [lsearch -exact $result "*"]
        if {$ndx != -1} {
            set result [lreplace $result $ndx $ndx]
        }

        foreach name [array names Snit_typemethodCache -glob $pattern] {
            if {[lsearch -exact $result $name] == -1} {
                lappend result $name
            }
        }
    }

    return $result
}

# Returns a list of the type's instances whose names match
# a pattern.
#
# type		A Snit type
# pattern       Optional.  The glob pattern to match
#               Defaults to *
#
# REQUIRE: type is fully qualified.

proc ::snit::RT.typemethod.info.instances {type {pattern *}} {
    set result {}

    foreach selfns [namespace children $type] {
        upvar ${selfns}::Snit_instance instance

        if {[string match $pattern $instance]} {
            lappend result $instance
        }
    }

    return $result
}

#-----------------------------------------------------------------------
# Instance Introspection

# Implements the standard "info" method.
#
# type		The snit type
# selfns        The instance's instance namespace
# win           The instance's original name
# self          The instance's current name
# command       The info subcommand
# args          All other arguments.

proc ::snit::RT.method.info {type selfns win self command args} {
    switch -exact $command {
        type        -
        vars        -
        options     -
        methods     -
        typevars    -
        typemethods {
            set errflag [catch {
                uplevel 1 [linsert $args 0 ::snit::RT.method.info.$command  			       $type $selfns $win $self]
            } result]

            if {$errflag} {
                global errorInfo
                return -code error -errorinfo $errorInfo $result
            } else {
                return $result
            }
        }
        default {
            # error "\"$self info $command\" is not defined"
            return -code error "\"$self info $command\" is not defined"
        }
    }
}

# $self info type
#
# Returns the instance's type
proc ::snit::RT.method.info.type {type selfns win self} {
    return $type
}

# $self info typevars
#
# Returns the instance's type's typevariables
proc ::snit::RT.method.info.typevars {type selfns win self {pattern *}} {
    return [RT.typemethod.info.typevars $type $pattern]
}

# $self info typemethods
#
# Returns the instance's type's typemethods
proc ::snit::RT.method.info.typemethods {type selfns win self {pattern *}} {
    return [RT.typemethod.info.typemethods $type $pattern]
}

# Returns a list of the instance's methods whose names match a 
# pattern.  If "delegate method *" is used, the list may
# not be complete.
#
# type		A Snit type
# selfns        The instance namespace
# win		The original instance name
# self          The current instance name
# pattern       Optional.  The glob pattern to match.  Defaults
#               to *.

proc ::snit::RT.method.info.methods {type selfns win self {pattern *}} {
    variable ${type}::Snit_methodInfo
    variable ${selfns}::Snit_methodCache

    # FIRST, get the explicit names, skipping prefixes.
    set result {}

    foreach name [array names Snit_methodInfo -glob $pattern] {
        if {[lindex $Snit_methodInfo($name) 0] != 1} {
            lappend result $name
        }
    }

    # NEXT, add any from the cache that aren't explicit.
    if {[info exists Snit_methodInfo(*)]} {
        # First, remove "*" from the list.
        set ndx [lsearch -exact $result "*"]
        if {$ndx != -1} {
            set result [lreplace $result $ndx $ndx]
        }

        foreach name [array names Snit_methodCache -glob $pattern] {
            if {[lsearch -exact $result $name] == -1} {
                lappend result $name
            }
        }
    }

    return $result
}

# $self info vars
#
# Returns the instance's instance variables
proc ::snit::RT.method.info.vars {type selfns win self {pattern *}} {
    set result {}
    foreach name [info vars "${selfns}::$pattern"] {
        set tail [namespace tail $name]
        if {![string match "Snit_*" $tail]} {
            lappend result $name
        }
    }

    return $result
}

# $self info options 
#
# Returns a list of the names of the instance's options
proc ::snit::RT.method.info.options {type selfns win self {pattern *}} {
    variable ${type}::Snit_optionInfo

    # First, get the local and explicitly delegated options
    set result [concat $Snit_optionInfo(local) $Snit_optionInfo(delegated)]

    # If "configure" works as for Tk widgets, add the resulting
    # options to the list.  Skip excepted options
    if {$Snit_optionInfo(starcomp) ne ""} {
        upvar ${selfns}::Snit_components Snit_components
        set logicalName $Snit_optionInfo(starcomp)
        set comp $Snit_components($logicalName)

        if {![catch {$comp configure} records]} {
            foreach record $records {
                set opt [lindex $record 0]
                if {[lsearch -exact $result $opt] == -1 &&
                    [lsearch -exact $Snit_optionInfo(except) $opt] == -1} {
                    lappend result $opt
                }
            }
        }
    }

    # Next, apply the pattern
    set names {}

    foreach name $result {
        if {[string match $pattern $name]} {
            lappend names $name
        }
    }

    return $names
}
if {![package vsatisfies [package provide Tcl] 8.4]} {return}
package ifneeded snit 1.0      [list source [file join $dir snit.tcl]]
    #source ./snit1.0/snit.tcl
    #source ./snit1.0/pkgIndex.tcl
    

package provide pdf4tcl::glyphnames 0.1

# this file is auto-generated, please do NOT edit!

namespace eval pdf4tcl {
	variable glyph_names

array set glyph_names {2167 Eightroman 2168 Nineroman 2169 Tenroman 215B oneeighth 2170 oneroman 2171 tworoman 215C threeeighths 2172 threeroman 215D fiveeighths 2173 fourroman 215E seveneighths 2174 fiveroman 2175 sixroman 2176 sevenroman 2177 eightroman 216A Elevenroman 2178 nineroman 2179 tenroman 216B Twelveroman 20A1 colonsign 20A2 cruzeiro 20A3 franc 20A4 lira 20A7 peseta 217A elevenroman 20A9 won 217B twelveroman 2190 arrowleft 2200 universal 2191 arrowup 2192 arrowright 2202 partialdiff 2193 arrowdown 2203 thereexists 2194 arrowboth 2195 arrowupdn FFE0 centmonospace 2205 emptyset FFE1 sterlingmonospace 2206 increment 2196 arrowupleft 2197 arrowupright 2207 nabla 20AA sheqelhebrew 2198 arrowdownright 2208 element FFE3 macronmonospace 2199 arrowdownleft 20AB dong 2209 notelementof FFE5 yenmonospace 2211 summation 20AC euro FFE6 wonmonospace 2212 minus 2213 minusplus 2215 divisionslash 2217 asteriskmath 220B suchthat 2220 angle 2219 bulletoperator 220C notcontains 2223 divides 220F product 2225 parallel 2226 notparallel 2227 logicaland 2228 logicalor 221A radical 2229 intersection 221D proportional 221E infinity 2234 therefore 221F rightangle 2235 because 2236 ratio 2237 proportion 222A union 222B integral 222C dblintegral 2243 asymptoticallyequal 222E contourintegral 2245 congruent 2248 approxequal 2250 approaches 223C tildeoperator 2251 geometricallyequal 2252 approxequalorimage 223D reversedtilde 2253 imageorapproximatelyequal 2260 notequal 224C allequal 2261 equivalence 2262 notidentical 2264 lessequal 2265 greaterequal 2266 lessoverequal 2267 greateroverequal 2270 notlessnorequal 2271 notgreaternorequal 2272 lessorequivalent 2273 greaterorequivalent 2276 lessorgreater 2277 greaterorless 226A muchless 226B muchgreater 2279 notgreaternorless 2280 notprecedes 2281 notsucceeds 2282 subset 226E notless 2283 superset 226F notgreater 2284 notsubset 2285 notsuperset 2286 subsetorequal 2287 supersetorequal 21A8 arrowupdownbase 227A precedes 227B succeeds 0E01 kokaithai 2302 house 0E02 khokhaithai 2303 control 0E03 khokhuatthai 0E04 khokhwaithai 21B5 carriagereturn 2295 pluscircle 0E05 khokhonthai 2305 projective 0E06 khorakhangthai 2296 minuscircle 2297 timescircle 0E07 ngonguthai 228A subsetnotequal 0E08 chochanthai 0E10 thothanthai 228B supersetnotequal 0E09 chochingthai 2299 circleot 21C0 harpoonrightbarbup 2310 revlogicalnot 0E11 thonangmonthothai 0E12 thophuthaothai 2312 arc 0E13 nonenthai 21C4 arrowrightoverleft 0E14 dodekthai 0E15 totaothai 21C5 arrowupleftofdown 0E16 thothungthai 21C6 arrowleftoverright 0E17 thothahanthai 0E18 thothongthai 0E0A chochangthai 2318 propellor 0E0B sosothai 21D0 arrowleftdbl 2320 integraltp 0E19 nonuthai 0E20 phosamphaothai 21D1 arrowdblup 0E0C chochoethai 21BC harpoonleftbarbup 2321 integralbt 0E21 momathai 0E0D yoyingthai 0E22 yoyakthai 21D2 dblarrowright 21D3 arrowdbldown 0E0E dochadathai 0E23 roruathai 0E0F topatakthai 21D4 dblarrowleft 0E24 ruthai 0E25 lolingthai 2325 option 2326 deleteright 0E26 luthai 0E27 wowaenthai 2327 clear 0E28 sosalathai 0E1A bobaimaithai 0E29 sorusithai 2329 angleleft 21E0 arrowdashleft 0E1B poplathai 0E30 saraathai 21E1 arrowdashup 0E31 maihanakatthai 0E1C phophungthai 21E2 arrowdashright 21CD arrowleftdblstroke 0E1D fofathai 0E32 saraaathai 21E3 arrowdashdown 0E1E phophanthai 0E33 saraamthai 21CF arrowrightdblstroke 21E4 arrowtableft 0E1F fofanthai 0E34 saraithai 21E5 arrowtabright 0E35 saraiithai 21E6 arrowleftwhite 0E36 sarauethai 21E7 arrowupwhite 0E37 saraueethai 0E2A sosuathai 232A angleright 21E8 arrowrightwhite 0E38 sarauthai 21E9 arrowdownwhite 232B deleteleft 0E2B hohipthai 0E40 saraethai 0E39 sarauuthai 0E2C lochulathai 0E41 saraaethai 0E2D oangthai 0E42 saraothai 0E2E honokhukthai 21DE pageup 0E43 saraaimaimuanthai 21DF pagedown 0E2F paiyannoithai 0E44 saraaimaimalaithai 0E45 lakkhangyaothai 0E46 maiyamokthai 0E47 maitaikhuthai 21EA capslock 0E48 maiekthai 0E3A phinthuthai 0E50 zerothai 0E49 maithothai 0E51 onethai 0E52 twothai 0E53 threethai 0E3F bahtthai 0E54 fourthai 0E55 fivethai 0E56 sixthai 0E57 seventhai 0E58 eightthai 0E4A maitrithai 0E4B maichattawathai 0E59 ninethai 0E4C thanthakhatthai 0E4D nikhahitthai 0E4E yamakkanthai 0E4F fongmanthai 0E5A angkhankhuthai 0E5B khomutthai 22A3 tackleft 22A4 tackdown 22A5 perpendicular 22C5 dotmath 2423 blank 22BF righttriangle 22CE curlyor 22CF curlyand 22DA lessequalorgreater 22DB greaterequalorless 22EE ellipsisvertical 2460 onecircle 2461 twocircle 2462 threecircle 2463 fourcircle 2464 fivecircle 2465 sixcircle 2466 sevencircle 2467 eightcircle 2468 ninecircle 2469 tencircle 2470 seventeencircle 2471 eighteencircle 2472 nineteencircle 2473 twentycircle 2474 oneparen 2475 twoparen 2476 threeparen 2477 fourparen 246A elevencircle 2478 fiveparen 246B twelvecircle 2480 thirteenparen 2479 sixparen 246C thirteencircle 2481 fourteenparen 2482 fifteenparen 246D fourteencircle 2483 sixteenparen 246E fifteencircle 246F sixteencircle 2484 seventeenparen 2485 eighteenparen 2486 nineteenparen 2487 twentyparen 2488 oneperiod 247A sevenparen 2489 twoperiod 2500 SF100000 247B eightparen 2490 nineperiod 2491 tenperiod 247C nineparen 247D tenparen 2502 SF110000 2492 elevenperiod 2493 twelveperiod 247E elevenparen 247F twelveparen 2494 thirteenperiod 2495 fourteenperiod 2496 fifteenperiod 2497 sixteenperiod 248A threeperiod 2498 seventeenperiod 2510 SF030000 2499 eighteenperiod 248B fourperiod 248C fiveperiod 248D sixperiod 248E sevenperiod 2514 SF020000 248F eightperiod 2518 SF040000 249A nineteenperiod 249B twentyperiod 250C SF010000 249C aparen 249D bparen 249E cparen 2524 SF090000 249F dparen 251C SF080000 2534 SF070000 252C SF060000 2550 SF430000 2551 SF240000 253C SF050000 {FEDF FEE4 FEA0} lammeemjeeminitialarabic 2552 SF510000 2553 SF520000 2554 SF390000 2555 SF220000 2556 SF210000 2557 SF250000 2558 SF500000 2559 SF490000 2560 SF420000 {FEDF FEE4 FEA8} lammeemkhahinitialarabic 2561 SF190000 2562 SF200000 2563 SF230000 2564 SF470000 2565 SF480000 2566 SF410000 2567 SF450000 2568 SF460000 255A SF380000 2569 SF400000 255B SF280000 255C SF270000 255D SF260000 255E SF360000 255F SF370000 256A SF540000 2580 upblock 256B SF530000 24A0 eparen 256C SF440000 24A1 fparen 24A2 gparen 24A3 hparen 2584 dnblock 24A4 iparen 24A5 jparen 24A6 kparen 24A7 lparen 2588 block 24A8 mparen 24B0 uparen 24A9 nparen 2590 rtblock 24B1 vparen 2591 shadelight 24B2 wparen 2592 shademedium 24B3 xparen 2593 shadedark 24B4 yparen 24B5 zparen 2605 blackstar 2606 whitestar 24B6 Acircle 24B7 Bcircle 24B8 Ccircle 24AA oparen 24C0 Kcircle 24B9 Dcircle 24AB pparen 24C1 Lcircle 258C lfblock 24AC qparen 24C2 Mcircle 24AD rparen 24AE sparen 24C3 Ncircle 24AF tparen 24C4 Ocircle 24C5 Pcircle 24C6 Qcircle 24C7 Rcircle 24C8 Scircle 24BA Ecircle 24D0 acircle 24C9 Tcircle 24BB Fcircle 24BC Gcircle 24D1 bcircle 24BD Hcircle 24D2 ccircle 260E telephoneblack 24BE Icircle 24D3 dcircle 260F whitetelephone 24BF Jcircle 24D4 ecircle 24D5 fcircle 24D6 gcircle 24D7 hcircle 24CA Ucircle 24D8 icircle 24CB Vcircle 24D9 jcircle 24E0 qcircle 24CC Wcircle 261C pointingindexleftwhite 24E1 rcircle 24CD Xcircle 261D pointingindexupwhite 24E2 scircle 24E3 tcircle 24CE Ycircle 261E pointingindexrightwhite 24E4 ucircle 24CF Zcircle 261F pointingindexdownwhite 24E5 vcircle 24E6 wcircle 24E7 xcircle 24E8 ycircle 24DA kcircle 24E9 zcircle 2640 venus 24DB lcircle 2641 earth 24DC mcircle 2642 mars 24DD ncircle 24DE ocircle 262F yinyang 24DF pcircle 263A whitesmilingface 263B invsmileface 263C sun 2660 spadesuitblack 2661 heartsuitwhite 2662 diamondsuitwhite 2663 clubsuitblack 2664 spadesuitwhite 2665 heartsuitblack 2666 diamond 2667 clubsuitwhite 2668 hotsprings 2669 quarternote 266A musicalnote 25A0 filledbox 266B musicalnotedbl 25A1 whitesquare 266C beamedsixteenthnotes 266D musicflatsign 25A3 squarewhitewithsmallblack 25A4 squarehorizontalfill 266F musicsharpsign 25A5 squareverticalfill 25A6 squareorthogonalcrosshatchfill 25A7 squareupperlefttolowerrightfill 25A8 squareupperrighttolowerleftfill 25A9 squarediagonalcrosshatchfill 25B2 triagup 25B3 whiteuppointingtriangle 25B4 blackuppointingsmalltriangle 25B5 whiteuppointingsmalltriangle 25B6 blackrightpointingtriangle 25B7 whiterightpointingtriangle 25AA blacksmallsquare 25B9 whiterightpointingsmalltriangle 25AB whitesmallsquare 25C0 blackleftpointingtriangle 25C1 whiteleftpointingtriangle 25AC filledrect 25C3 whiteleftpointingsmalltriangle 2713 checkmark 25C4 triaglf 25C6 blackdiamond 25C7 whitediamond 25C8 whitediamondcontainingblacksmalldiamond 25BA triagrt 25D0 circlewithlefthalfblack 25C9 fisheye 25BC triagdn 25D1 circlewithrighthalfblack 25BD whitedownpointingtriangle 25BF whitedownpointingsmalltriangle 25D8 invbullet 25CA lozenge 25CB whitecircle 25D9 whitecircleinverse 25CC dottedcircle 25E2 blacklowerrighttriangle 25E3 blacklowerlefttriangle 25CE bullseye 25CF blackcircle 25E4 blackupperlefttriangle 25E5 blackupperrighttriangle 25E6 whitebullet 25EF largecircle 2790 sevencircleinversesansserif 2791 eightcircleinversesansserif 2792 ninecircleinversesansserif 278A onecircleinversesansserif 278B twocircleinversesansserif 278C threecircleinversesansserif 278D fourcircleinversesansserif 278E fivecircleinversesansserif 278F sixcircleinversesansserif 279E arrowrightheavy 3000 ideographicspace 3001 ideographiccomma 3002 ideographicperiod 3003 dittomark 3004 jis 3005 ideographiciterationmark 3006 ideographicclose 3007 ideographiczero 3008 anglebracketleft 3009 anglebracketright 3010 blacklenticularbracketleft 3011 blacklenticularbracketright 3012 postalmark 3013 getamark 3014 tortoiseshellbracketleft 3015 tortoiseshellbracketright 3016 whitelenticularbracketleft 3017 whitelenticularbracketright 3018 whitetortoiseshellbracketleft 300A dblanglebracketleft 3019 whitetortoiseshellbracketright 300B dblanglebracketright 3020 postalmarkface 300C cornerbracketleft 3021 onehangzhou 3022 twohangzhou 300D cornerbracketright 300E whitecornerbracketleft 3023 threehangzhou 300F whitecornerbracketright 3024 fourhangzhou 3025 fivehangzhou 3026 sixhangzhou 3027 sevenhangzhou 3028 eighthangzhou 3029 ninehangzhou 301C wavedash 301D quotedblprimereversed 301E quotedblprime 3036 circlepostalmark 3041 asmallhiragana 3042 ahiragana 3043 ismallhiragana 3044 ihiragana 3045 usmallhiragana 3046 uhiragana 3047 esmallhiragana 3048 ehiragana 3050 guhiragana 3049 osmallhiragana 3051 kehiragana 3052 gehiragana 3053 kohiragana 3054 gohiragana 3055 sahiragana 3056 zahiragana 3057 sihiragana 3058 zihiragana 304A ohiragana 3059 suhiragana 3060 dahiragana 304B kahiragana 3061 tihiragana 304C gahiragana 3062 dihiragana 304D kihiragana 3063 tusmallhiragana 304E gihiragana 3064 tuhiragana 304F kuhiragana 3065 duhiragana 3066 tehiragana 3067 dehiragana 305A zuhiragana 3068 tohiragana 3070 bahiragana 3069 dohiragana 305B sehiragana 305C zehiragana 3071 pahiragana 305D sohiragana 3072 hihiragana 305E zohiragana 3073 bihiragana 305F tahiragana 3074 pihiragana 3075 huhiragana 3076 buhiragana 3077 puhiragana 3078 hehiragana 306A nahiragana 3079 behiragana 3080 muhiragana 306B nihiragana 3081 mehiragana 306C nuhiragana 3082 mohiragana 306D nehiragana 3083 yasmallhiragana 306E nohiragana 3084 yahiragana 306F hahiragana 3085 yusmallhiragana {05E7 05B0} qofshevahebrew 3086 yuhiragana {05E7 05B1} qofhatafsegolhebrew 3087 yosmallhiragana {05E7 05B2} qofhatafpatahhebrew 3088 yohiragana 307A pehiragana 3090 wihiragana 307B hohiragana {05E7 05B4} qofhiriqhebrew 3089 rahiragana 3091 wehiragana 307C bohiragana {05E7 05B5} qoftserehebrew 3092 wohiragana 307D pohiragana {05E7 05B6} qofsegolhebrew 307E mahiragana 3093 nhiragana {05E7 05B7} qofpatahhebrew 3094 vuhiragana 307F mihiragana {05E7 05B8} qofqamatshebrew 3105 bbopomofo {05E7 05B9} qofholamhebrew 3106 pbopomofo 3107 mbopomofo 3108 fbopomofo 308A rihiragana 3109 dbopomofo 3110 jbopomofo 308B ruhiragana 3111 qbopomofo 308C rehiragana 3112 xbopomofo 308D rohiragana 3113 zhbopomofo 308E wasmallhiragana 308F wahiragana 3114 chbopomofo {05E7 05BB} qofqubutshebrew 3115 shbopomofo 3116 rbopomofo 3117 zbopomofo 310A tbopomofo 3118 cbopomofo 309B voicedmarkkana 3120 aubopomofo 310B nbopomofo 3119 sbopomofo 310C lbopomofo 3121 oubopomofo 309C semivoicedmarkkana 3122 anbopomofo 310D gbopomofo 309D iterationhiragana 309E voicediterationhiragana 3123 enbopomofo 310E kbopomofo 3124 angbopomofo 310F hbopomofo 3125 engbopomofo 3126 erbopomofo 3127 ibopomofo 3128 ubopomofo 311A abopomofo 3129 iubopomofo 311B obopomofo 311C ebopomofo 3131 kiyeokkorean 3132 ssangkiyeokkorean 311D ehbopomofo 311E aibopomofo 3133 kiyeoksioskorean 311F eibopomofo 3134 nieunkorean 3135 nieuncieuckorean 3136 nieunhieuhkorean 3137 tikeutkorean 3138 ssangtikeutkorean 3140 rieulhieuhkorean 3139 rieulkorean 3141 mieumkorean 3142 pieupkorean 3143 ssangpieupkorean 3144 pieupsioskorean 3145 sioskorean 3146 ssangsioskorean 3147 ieungkorean 3148 cieuckorean 313A rieulkiyeokkorean 3149 ssangcieuckorean 3150 aekorean 313B rieulmieumkorean 3151 yakorean 313C rieulpieupkorean 3152 yaekorean 313D rieulsioskorean 3153 eokorean 313E rieulthieuthkorean 3154 ekorean 313F rieulphieuphkorean 3155 yeokorean 3156 yekorean 3157 okorean 3158 wakorean 314A chieuchkorean 3160 yukorean 3159 waekorean 314B khieukhkorean 314C thieuthkorean 3161 eukorean 3162 yikorean 314D phieuphkorean 314E hieuhkorean 3163 ikorean 314F akorean 3164 hangulfiller 3165 ssangnieunkorean 3166 nieuntikeutkorean 3167 nieunsioskorean 3168 nieunpansioskorean 315A oekorean 315B yokorean 3170 mieumpansioskorean 3169 rieulkiyeoksioskorean 315C ukorean 3171 kapyeounmieumkorean 315D weokorean 3172 pieupkiyeokkorean 315E wekorean 3173 pieuptikeutkorean 315F wikorean 3174 pieupsioskiyeokkorean 3175 pieupsiostikeutkorean 3176 pieupcieuckorean 3177 pieupthieuthkorean 3178 kapyeounpieupkorean 316A rieultikeutkorean 3180 ssangieungkorean 3179 kapyeounssangpieupkorean 316B rieulpieupsioskorean 3181 yesieungkorean 30A1 asmallkatakana 316C rieulpansioskorean 3182 yesieungsioskorean 30A2 akatakana 316D rieulyeorinhieuhkorean 3183 yesieungpansioskorean 30A3 ismallkatakana 316E mieumpieupkorean 30A4 ikatakana 3184 kapyeounphieuphkorean 316F mieumsioskorean 30A5 usmallkatakana 3185 ssanghieuhkorean 3186 yeorinhieuhkorean 30A6 ukatakana 3187 yoyakorean 30A7 esmallkatakana 3188 yoyaekorean 317A sioskiyeokkorean 30A8 ekatakana 3189 yoikorean 317B siosnieunkorean 30B0 gukatakana 3200 kiyeokparenkorean 30A9 osmallkatakana 317C siostikeutkorean 30B1 kekatakana 3201 nieunparenkorean 3202 tikeutparenkorean 317D siospieupkorean 30B2 gekatakana 317E sioscieuckorean 30B3 kokatakana 3203 rieulparenkorean 30B4 gokatakana 3204 mieumparenkorean 317F pansioskorean 3205 pieupparenkorean 30B5 sakatakana 30B6 zakatakana 3206 siosparenkorean 30B7 sikatakana 3207 ieungparenkorean 30B8 zikatakana 318A yuyeokorean 3208 cieucparenkorean 30AA okatakana 318B yuyekorean 3210 tikeutaparenkorean 30B9 sukatakana 3209 chieuchparenkorean 30C0 dakatakana 30AB kakatakana 318C yuikorean 30C1 tikatakana 30AC gakatakana 3211 rieulaparenkorean 318D araeakorean 30C2 dikatakana 30AD kikatakana 3212 mieumaparenkorean 30C3 tusmallkatakana 318E araeaekorean 30AE gikatakana 3213 pieupaparenkorean 30C4 tukatakana 3214 siosaparenkorean 30AF kukatakana 30C5 dukatakana 3215 ieungaparenkorean 30C6 tekatakana 3216 cieucaparenkorean 3217 chieuchaparenkorean 30C7 dekatakana 30BA zukatakana 30C8 tokatakana 3218 khieukhaparenkorean 320A khieukhparenkorean 320B thieuthparenkorean 3219 thieuthaparenkorean 30D0 bakatakana 30C9 dokatakana 3220 oneideographicparen 30BB sekatakana 30BC zekatakana 3221 twoideographicparen 30D1 pakatakana 320C phieuphparenkorean 3222 threeideographicparen 30BD sokatakana 320D hieuhparenkorean 30D2 hikatakana 30BE zokatakana 30D3 bikatakana 3223 fourideographicparen 320E kiyeokaparenkorean 30BF takatakana 3224 fiveideographicparen 320F nieunaparenkorean 30D4 pikatakana 3225 sixideographicparen 30D5 hukatakana 30D6 bukatakana 3226 sevenideographicparen 3227 eightideographicparen 30D7 pukatakana 30D8 hekatakana 30CA nakatakana 3228 nineideographicparen 321A phieuphaparenkorean 3229 tenideographicparen 30D9 bekatakana 321B hieuhaparenkorean 3230 ideographicsunparen 30E0 mukatakana 30CB nikatakana 321C cieucuparenkorean 3231 ideographicstockparen 30E1 mekatakana 30CC nukatakana 3232 ideographichaveparen 30E2 mokatakana 30CD nekatakana 30E3 yasmallkatakana 3233 ideographicsocietyparen 30CE nokatakana 30E4 yakatakana 30CF hakatakana 3234 ideographicnameparen 30E5 yusmallkatakana 3235 ideographicspecialparen 30E6 yukatakana 3236 ideographicfinancialparen 30E7 yosmallkatakana 3237 ideographiccongratulationparen 30E8 yokatakana 3238 ideographiclaborparen 322A ideographicmoonparen 30DA pekatakana 30F0 wikatakana 30DB hokatakana 3240 ideographicfestivalparen 322B ideographicfireparen 3239 ideographicrepresentparen 30E9 rakatakana 30F1 wekatakana 30DC bokatakana 322C ideographicwaterparen 30F2 wokatakana 3242 ideographicselfparen 322D ideographicwoodparen 30DD pokatakana {0621 0650} hamzalowkasraarabic 322E ideographicmetalparen 3243 ideographicreachparen 30DE makatakana 30F3 nkatakana 30F4 vukatakana 322F ideographicearthparen 30DF mikatakana {0621 0652} hamzasukunarabic 30F5 kasmallkatakana 30F6 kesmallkatakana 30F7 vakatakana 30F8 vikatakana 323A ideographiccallparen 30EA rikatakana 30F9 vekatakana 323B ideographicstudyparen 30EB rukatakana 323C ideographicsuperviseparen 30EC rekatakana 323D ideographicenterpriseparen 30ED rokatakana 30EE wasmallkatakana {0621 064B} hamzafathatanarabic 323E ideographicresourceparen 30EF wakatakana {0621 064C} hamzadammatanarabic 323F ideographicallianceparen {0621 064D} hamzalowkasratanarabic {0621 064E} hamzafathaarabic {0621 064F} hamzadammaarabic 30FA vokatakana 30FB dotkatakana 3260 kiyeokcirclekorean 3261 nieuncirclekorean 30FC prolongedkana 3262 tikeutcirclekorean 30FD iterationkatakana 30FE voicediterationkatakana 3263 rieulcirclekorean 3264 mieumcirclekorean 3265 pieupcirclekorean 3266 sioscirclekorean 3267 ieungcirclekorean 3268 cieuccirclekorean 3270 tikeutacirclekorean 3269 chieuchcirclekorean 3271 rieulacirclekorean 3272 mieumacirclekorean 3273 pieupacirclekorean 3274 siosacirclekorean 3275 ieungacirclekorean 3276 cieucacirclekorean 3277 chieuchacirclekorean 3278 khieukhacirclekorean 326A khieukhcirclekorean 326B thieuthcirclekorean 3279 thieuthacirclekorean 326C phieuphcirclekorean 326D hieuhcirclekorean 326E kiyeokacirclekorean 326F nieunacirclekorean 327A phieuphacirclekorean 1E00 Aringbelow 3300 apaatosquare 327B hieuhacirclekorean 3290 ideographsuncircle 1E01 aringbelow 1E02 Bdotaccent 3303 aarusquare 1E03 bdotaccent 1E04 Bdotbelow 3294 ideographnamecircle 327F koreanstandardsymbol 1E05 bdotbelow 3305 intisquare 1E06 Blinebelow 3296 ideographicfinancialcircle 1E07 blinebelow 1E08 Ccedillaacute 3298 ideographiclaborcircle 328A ideographmooncircle 1E10 Dcedilla 1E09 ccedillaacute 328B ideographfirecircle 3299 ideographicsecretcircle 1E11 dcedilla 328C ideographwatercircle 1E12 Dcircumflexbelow 328D ideographwoodcircle 1E13 dcircumflexbelow 328E ideographmetalcircle 1E14 Emacrongrave 328F ideographearthcircle 3314 kirosquare 1E15 emacrongrave 3315 kiroguramusquare 1E16 Emacronacute 3316 kiromeetorusquare 1E17 emacronacute 1E0A Ddotaccent 1E18 Ecircumflexbelow 3318 guramusquare 1E20 Gmacron 1E0B ddotaccent 1E19 ecircumflexbelow 1E0C Ddotbelow 1E21 gmacron 1E22 Hdotaccent 1E0D ddotbelow 329D ideographicexcellentcircle 330D karoriisquare 3322 sentisquare 1E0E Dlinebelow 1E23 hdotaccent 329E ideographicprintcircle 3323 sentosquare 1E24 Hdotbelow 1E0F dlinebelow 1E25 hdotbelow 1E26 Hdieresis 3326 dorusquare 3327 tonsquare 1E27 hdieresis 1E28 Hcedilla 1E1A Etildebelow 1E30 Kacute 1E1B etildebelow 1E29 hcedilla 1E1C Ecedillabreve 3331 birusquare 1E31 kacute 1E32 Kdotbelow 1E1D ecedillabreve 1E1E Fdotaccent 3333 huiitosquare 1E33 kdotbelow 331E kooposquare 1E34 Klinebelow 1E1F fdotaccent 1E35 klinebelow 1E36 Ldotbelow 3336 hekutaarusquare 1E37 ldotbelow 1E38 Ldotbelowmacron 1E2A Hbrevebelow 332A haitusquare 1E40 Mdotaccent 1E2B hbrevebelow 3339 herutusquare 1E39 ldotbelowmacron 332B paasentosquare 1E2C Itildebelow 1E41 mdotaccent 1E42 Mdotbelow 3342 hoonsquare 1E2D itildebelow 1E2E Idieresisacute 1E43 mdotbelow 1E44 Ndotaccent 1E2F idieresisacute 1E45 ndotaccent 1E46 Ndotbelow 3347 mansyonsquare 1E47 ndotbelow 1E48 Nlinebelow 1E3A Llinebelow 1E50 Omacrongrave 1E3B llinebelow 3349 mirisquare 1E49 nlinebelow 333B peezisquare 1E3C Lcircumflexbelow 1E51 omacrongrave 3351 rittorusquare 1E52 Omacronacute 1E3D lcircumflexbelow 1E3E Macute 1E53 omacronacute 1E54 Pacute 1E3F macute 1E55 pacute 1E56 Pdotaccent 3357 wattosquare 1E57 pdotaccent 1E58 Rdotaccent 1E4A Ncircumflexbelow 334A miribaarusquare 1E60 Sdotaccent 1E4B ncircumflexbelow 1E59 rdotaccent 1E4C Otildeacute 1E61 sdotaccent 1E62 Sdotbelow 334D meetorusquare 1E4D otildeacute 334E yaadosquare 1E4E Otildedieresis 1E63 sdotbelow 1E64 Sacutedotaccent 1E4F otildedieresis 1E65 sacutedotaccent 1E66 Scarondotaccent 1E67 scarondotaccent 1E68 Sdotbelowdotaccent 1E5A Rdotbelow 1E70 Tcircumflexbelow 1E5B rdotbelow 1E69 sdotbelowdotaccent 1E71 tcircumflexbelow 1E5C Rdotbelowmacron 1E72 Udieresisbelow 1E5D rdotbelowmacron 1E73 udieresisbelow 1E5E Rlinebelow 1E74 Utildebelow 1E5F rlinebelow 1E75 utildebelow 1E76 Ucircumflexbelow 1E77 ucircumflexbelow 1E78 Utildeacute 1E6A Tdotaccent 1E79 utildeacute 1E6B tdotaccent 1E80 Wgrave 3380 paampssquare 1E81 wgrave 1E6C Tdotbelow 3381 nasquare 1E6D tdotbelow 1E82 Wacute 3382 muasquare 1E83 wacute 1E6E Tlinebelow 32A3 ideographiccorrectcircle 3383 masquare 1E6F tlinebelow 1E84 Wdieresis 32A4 ideographichighcircle 3384 kasquare 1E85 wdieresis 3385 KBsquare 32A5 ideographiccentrecircle 1E86 Wdotaccent 3386 MBsquare 32A6 ideographiclowcircle 1E87 wdotaccent 3387 GBsquare 32A7 ideographicleftcircle 1E88 Wdotbelow 1E7A Umacrondieresis 3388 calsquare 32A8 ideographicrightcircle 1E89 wdotbelow 1E7B umacrondieresis 1E90 Zcircumflex 3390 Hzsquare 337B heiseierasquare 32A9 ideographicmedicinecircle 3389 kcalsquare 1E91 zcircumflex 337C syouwaerasquare 1E7C Vtilde 3391 khzsquare 1E7D vtilde 337D taisyouerasquare 1E92 Zdotbelow 3392 mhzsquare 1E93 zdotbelow 1E7E Vdotbelow 3393 ghzsquare 337E meizierasquare 1E7F vdotbelow 3394 thzsquare 1E94 Zlinebelow 337F corporationsquare 1E95 zlinebelow 3395 mulsquare 1E96 hlinebelow 3396 mlsquare 1E97 tdieresis 3397 dlsquare 1E98 wring 1E8A Xdotaccent 3398 klsquare 338A pfsquare 1E99 yring 1E8B xdotaccent 3399 fmsquare 338B nfsquare 1E8C Xdieresis 338C mufsquare 1E8D xdieresis 338D mugsquare 338E squaremg 1E8E Ydotaccent 1E8F ydotaccent 338F squarekg 1E9A arighthalfring 339A nmsquare 1E9B slongdotaccent 339B mumsquare 339C squaremm 339D squarecm 339E squarekm 339F mmsquaredsquare 1EA0 Adotbelow 33A0 cmsquaredsquare 33A1 squaremsquared 1EA1 adotbelow 1EA2 Ahookabove 33A2 kmsquaredsquare 1EA3 ahookabove 33A3 mmcubedsquare 1EA4 Acircumflexacute 33A4 cmcubedsquare 1EA5 acircumflexacute 33A5 mcubedsquare 1EA6 Acircumflexgrave 33A6 kmcubedsquare 1EA7 acircumflexgrave 33A7 moverssquare 1EA8 Acircumflexhookabove 33A8 moverssquaredsquare 1EA9 acircumflexhookabove 1EB0 Abrevegrave 33A9 pasquare 33B0 pssquare 1EB1 abrevegrave 33B1 nssquare 1EB2 Abrevehookabove 33B2 mussquare 1EB3 abrevehookabove 33B3 mssquare 1EB4 Abrevetilde 33B4 pvsquare 1EB5 abrevetilde 33B5 nvsquare {FB7C FEE4} tchehmeeminitialarabic 1EB6 Abrevedotbelow 33B6 muvsquare 1EB7 abrevedotbelow 33B7 mvsquare 1EAA Acircumflextilde 1EB8 Edotbelow 33AA kpasquare 33B8 kvsquare 1EAB acircumflextilde 1EC0 Ecircumflexgrave 1EB9 edotbelow 33C0 kohmsquare 33AB mpasquare 33B9 mvmegasquare 1EAC Acircumflexdotbelow 1EC1 ecircumflexgrave 33AC gpasquare 33C1 mohmsquare 1EAD acircumflexdotbelow 1EC2 Ecircumflexhookabove 33C2 amsquare 33AD radsquare 1EAE Abreveacute 33C3 bqsquare 1EC3 ecircumflexhookabove 33AE radoverssquare 33C4 squarecc 1EAF abreveacute 1EC4 Ecircumflextilde 33AF radoverssquaredsquare 33C5 cdsquare 1EC5 ecircumflextilde 1EC6 Ecircumflexdotbelow 33C6 coverkgsquare 33C7 cosquare 1EC7 ecircumflexdotbelow 1EC8 Ihookabove 1EBA Ehookabove 33C8 dbsquare 33BA pwsquare 1ED0 Ocircumflexacute 1EBB ehookabove 33C9 gysquare 1EC9 ihookabove 33D0 lmsquare 33BB nwsquare 33D1 squareln 1EBC Etilde 33BC muwsquare 1ED1 ocircumflexacute 33D2 squarelog 1ED2 Ocircumflexgrave 1EBD etilde 33BD mwsquare 1EBE Ecircumflexacute 33BE kwsquare 33D3 lxsquare 1ED3 ocircumflexgrave 1ED4 Ocircumflexhookabove 1EBF ecircumflexacute 33D4 mbsquare 33BF mwmegasquare 33D5 squaremil 1ED5 ocircumflexhookabove 1ED6 Ocircumflextilde 33D6 molsquare 1ED7 ocircumflextilde 1ED8 Ocircumflexdotbelow 1ECA Idotbelow 33CA hasquare 33D8 pmsquare 1EE0 Ohorntilde 33CB HPsquare 1ECB idotbelow 1ED9 ocircumflexdotbelow 1ECC Odotbelow 1EE1 ohorntilde 1EE2 Ohorndotbelow 33CD KKsquare 1ECD odotbelow 33CE squarekmcapital 1ECE Ohookabove 1EE3 ohorndotbelow 1EE4 Udotbelow 33CF ktsquare 1ECF ohookabove 1EE5 udotbelow 1EE6 Uhookabove 1EE7 uhookabove 1EE8 Uhornacute 1EDA Ohornacute 1EE9 uhornacute 33DB srsquare 1EF0 Uhorndotbelow 1EDB ohornacute 1EF1 uhorndotbelow 33DC svsquare 1EDC Ohorngrave 33DD wbsquare 1EF2 Ygrave 1EDD ohorngrave 1EF3 ygrave 1EDE Ohornhookabove 1EF4 Ydotbelow 1EDF ohornhookabove 1EF5 ydotbelow 1EF6 Yhookabove 1EF7 yhookabove 1EF8 Ytilde 1EEA Uhorngrave 1EF9 ytilde 1EEB uhorngrave 1EEC Uhornhookabove 1EED uhornhookabove 1EEE Uhorntilde 1EEF uhorntilde {05DC 05B9 05BC} lamedholamdageshhebrew {05DC 05B9} lamedholamhebrew {0651 064B} shaddafathatanarabic 5344 twentyhangzhou {05DA 05B0} finalkafshevahebrew {05E8 05B0} reshshevahebrew {05E8 05B1} reshhatafsegolhebrew {05E8 05B2} reshhatafpatahhebrew {05E8 05B4} reshhiriqhebrew {05E8 05B5} reshtserehebrew {05E8 05B6} reshsegolhebrew {05E8 05B7} reshpatahhebrew {05DA 05B8} finalkafqamatshebrew {05E8 05B8} reshqamatshebrew {05E8 05B9} reshholamhebrew {05E8 05BB} reshqubutshebrew 0001 controlSTX 0002 controlSOT 0003 controlETX 0004 controlEOT 0005 controlENQ 0006 controlACK 0007 controlBEL 0008 controlBS 0010 controlDLE 0009 controlHT 0011 controlDC1 0012 controlDC2 0013 controlDC3 0014 controlDC4 0015 controlNAK 0016 controlSYN 0017 controlETB 0018 controlCAN 000A controlLF 0020 spacehackarabic 0019 controlEM 000B controlVT 000C controlFF 0021 exclam 000D controlCR 0022 quotedbl 000E controlSO 0023 numbersign 000F controlSI 0024 dollar 0025 percent 0026 ampersand 0027 quotesingle 001A controlSUB 0028 parenleft 0030 zero 001B controlESC 0029 parenright 001C controlFS 0031 one 0032 two 001D controlGS 0033 three 001E controlRS 001F controlUS 0034 four 0035 five 0036 six 0037 seven 002A asterisk 0038 eight 0040 at 0039 nine 002B plus 0041 A 002C comma 0042 B 002D hyphen 0043 C 002E period 002F slash 0044 D 0045 E 0046 F 0047 G 0048 H 003A colon 0050 P 0049 I 003B semicolon 0051 Q 003C less 0052 R 003D equal 0053 S 003E greater 0054 T 003F question 0055 U 0056 V 0057 W 0058 X 004A J 0059 Y 004B K 0060 grave 0061 a 004C L 004D M 0062 b 004E N 0063 c 004F O 0064 d 0065 e 0066 f 0067 g 005A Z 0068 h 005B bracketleft 0069 i 0070 p 005C backslash 0071 q 005D bracketright 0072 r 005E asciicircum 0073 s 005F underscore 0074 t 0075 u 0076 v 0077 w 0078 x 006A j 0079 y 006B k 006C l 006D m 006E n 006F o 007A z 0100 Amacron 007B braceleft 0101 amacron 007C verticalbar 0102 Abreve 007D braceright 0103 abreve 007E asciitilde 0104 Aogonek 007F controlDEL 0105 aogonek 0106 Cacute 0107 cacute 0108 Ccircumflex 0110 Dslash 0109 ccircumflex 0111 dmacron 0112 Emacron 0113 emacron 0114 Ebreve 0115 ebreve 0116 Edotaccent 0117 edotaccent 010A Cdotaccent 0118 Eogonek 0120 Gdotaccent 010B cdotaccent 0119 eogonek 010C Ccaron 0121 gdotaccent 0122 Gcommaaccent 010D ccaron 010E Dcaron 0123 gcommaaccent 0124 Hcircumflex 010F dcaron 0125 hcircumflex 0126 Hbar 0127 hbar 0128 Itilde 011A Ecaron 0130 Idotaccent 011B ecaron 0129 itilde 011C Gcircumflex 0131 dotlessi 0132 IJ 011D gcircumflex 011E Gbreve 0133 ij 0134 Jcircumflex 011F gbreve 0135 jcircumflex 0136 Kcommaaccent 0137 kcommaaccent 012A Imacron 0138 kgreenlandic 0139 Lacute 012B imacron 0140 ldotaccent 0141 Lslash 012C Ibreve 012D ibreve 0142 lslash 0143 Nacute 012E Iogonek 012F iogonek 0144 nacute 0145 Ncommaaccent 0146 ncommaaccent 0147 Ncaron 013A lacute 0148 ncaron 0150 Ohungarumlaut 013B Lcommaaccent 0149 quoterightn 013C lcommaaccent 0151 ohungarumlaut 0152 OE 013D Lcaron 013E lcaron 0153 oe 0154 Racute 013F Ldotaccent 0155 racute 0156 Rcommaaccent 0157 rcommaaccent 0158 Rcaron 014A Eng 0160 Scaron 014B eng 0159 rcaron 014C Omacron 0161 scaron 0162 Tcommaaccent 014D omacron 0163 tcommaaccent 014E Obreve 0164 Tcaron 014F obreve 0165 tcaron 0166 Tbar 0167 tbar 0168 Utilde 015A Sacute 0169 utilde 0170 Uhungarumlaut 015B sacute 0171 uhungarumlaut 015C Scircumflex 0172 Uogonek 015D scircumflex 0173 uogonek 015E Scedilla 0174 Wcircumflex 015F scedilla 0175 wcircumflex 0176 Ycircumflex 0177 ycircumflex 0178 Ydieresis 016A Umacron 016B umacron 0179 Zacute 0180 bstroke 00A0 nonbreakingspace 016C Ubreve 0181 Bhook 00A1 exclamdown 016D ubreve 0182 Btopbar 00A2 cent 00A3 sterling 016E Uring 0183 btopbar 016F uring 0184 Tonesix 00A4 currency 00A5 yen 0185 tonesix 0186 Oopen 00A6 brokenbar 0187 Chook 00A7 section 017A zacute 0188 chook 00A8 dieresis 017B Zdotaccent 0200 Adblgrave 0189 Dafrican 0190 Eopen 00A9 copyright 00B0 degree 017C zdotaccent 0201 adblgrave 0191 Fhook 00B1 plusminus 00B2 twosuperior 017D Zcaron 0202 Ainvertedbreve 0192 florin 017E zcaron 00B3 threesuperior 0193 Ghook 0203 ainvertedbreve 00B4 acute 0194 Gammaafrican 0204 Edblgrave 017F slong 0205 edblgrave 0195 hv 00B5 mu1 0196 Iotaafrican 0206 Einvertedbreve 00B6 paragraph 0197 Istroke 0207 einvertedbreve 00B7 periodcentered 0198 Khook 0208 Idblgrave 018A Dhook 00B8 cedilla 00AA ordfeminine 0210 Rdblgrave 00C0 Agrave 018B Dtopbar 00AB guillemotleft 0209 idblgrave 0199 khook 00B9 onesuperior 00C1 Aacute 018C dtopbar 00AC logicalnot 0211 rdblgrave 0212 Rinvertedbreve 00C2 Acircumflex 018D deltaturned 00AD softhyphen 00C3 Atilde 018E Ereversed 00AE registered 0213 rinvertedbreve 0214 Udblgrave 018F Schwa 00C4 Adieresis 00AF overscore 0215 udblgrave 00C5 Aring 0216 Uinvertedbreve 00C6 AE 0217 uinvertedbreve 00C7 Ccedilla 0218 Scommaaccent 020A Iinvertedbreve 00C8 Egrave 019A lbar 00BA ordmasculine 00C9 Eacute 00D0 Eth 00BB guillemotright 020B iinvertedbreve 019B lambdastroke 0219 scommaaccent 020C Odblgrave 00D1 Ntilde 019C Mturned 00BC onequarter 00D2 Ograve 019D Nhookleft 020D odblgrave 00BD onehalf 00BE threequarters 020E Oinvertedbreve 00D3 Oacute 019E nlegrightlong 00D4 Ocircumflex 019F Ocenteredtilde 020F oinvertedbreve 00BF questiondown 00D5 Otilde 00D6 Odieresis 00D7 multiply 00D8 Oslash 00CA Ecircumflex 00D9 Ugrave 00CB Edieresis 00E0 agrave 00E1 aacute 00CC Igrave 00E2 acircumflex 00CD Iacute 00CE Icircumflex 00E3 atilde 00E4 adieresis 00CF Idieresis 00E5 aring 00E6 ae 00E7 ccedilla 00DA Uacute 00E8 egrave 00DB Ucircumflex 00E9 eacute 00F0 eth 00DC Udieresis 00F1 ntilde 00DD Yacute 00F2 ograve 00DE Thorn 00F3 oacute 00DF germandbls 00F4 ocircumflex 00F5 otilde 00F6 odieresis 00F7 divide 00EA ecircumflex 00F8 oslash 00F9 ugrave 0250 aturned 00EB edieresis 0251 ascript 00EC igrave 0252 ascriptturned 00ED iacute 0253 bhook 00EE icircumflex 00EF idieresis 0254 oopen 0255 ccurl 0256 dtail 0257 dhook 00FA uacute 0258 ereversed 00FB ucircumflex 0260 ghook 0259 schwa 00FC udieresis 0261 gscript 00FD yacute 00FE thorn 0263 gammalatinsmall 00FF ydieresis 0264 ramshorn 0265 hturned 0266 hhook 0267 henghook 0268 istroke 025A schwahook 025B eopen 0269 iotalatin 0270 mlonglegturned 025C eopenreversed 0271 mhook 025D eopenreversedhook 0272 nhookleft 025E eopenreversedclosed 0273 nhookretroflex 025F jdotlessstroke 0275 obarred 0277 omegalatinclosed 0278 philatin 01A0 Ohorn 026B lmiddletilde 0279 rturned 0281 Rsmallinverted 026C lbelt 01A1 ohorn 0282 shook 01A2 Oi 026D lhookretroflex 0283 esh 026E lezh 01A3 oi 01A4 Phook 0284 dotlessjstrokehook 026F mturned 0285 eshsquatreversed 01A5 phook 01A6 yr 0286 eshcurl F721 exclamsmall 0287 tturned 01A7 Tonetwo 0288 tretroflexhook 01A8 tonetwo 027A rlonglegturned 0290 zretroflexhook 01B0 uhorn 0289 ubar 01A9 Esh F724 dollaroldstyle 0300 gravecomb 027B rhookturned 0291 zcurl 0301 acutecomb 01B1 Upsilonafrican 027C rlongleg 01B2 Vhook F726 ampersandsmall 0302 circumflexcmb 0292 ezh 027D rhook 0303 tildecomb 01B3 Yhook 0293 ezhcurl 027E rfishhook 01B4 yhook 0294 glottalstop 0304 macroncmb 027F rfishhookreversed F730 zerooldstyle 01B5 Zstroke 0295 glottalstopreversed 0305 overlinecmb 01B6 zstroke 0306 brevecmb 0296 glottalstopinverted F731 oneoldstyle F732 twooldstyle 01B7 Ezh 0297 cstretched 0307 dotaccentcmb 028A upsilonlatin F733 threeoldstyle 01B8 Ezhreversed 0298 bilabialclick 0308 dieresiscmb 01AA eshreversedloop 028B vhook 01AB tpalatalhook 0310 candrabinducmb 01C0 clickdental 01B9 ezhreversed F734 fouroldstyle 0309 hookcmb 028C vturned 01AC Thook 0311 breveinvertedcmb 01C1 clicklateral F735 fiveoldstyle 028D wturned 01AD thook F736 sixoldstyle 01C2 clickalveolar 0312 commaturnedabovecmb 028E yturned 01AE Tretroflexhook 01C3 clickretroflex 0313 commaabovecmb F737 sevenoldstyle 01AF Uhorn 01C4 DZcaron 0314 commareversedabovecmb F738 eightoldstyle 01C5 Dzcaron 0315 commaaboverightcmb F739 nineoldstyle 01C6 dzcaron 0316 gravebelowcmb 0317 acutebelowcmb 01C7 LJ 01C8 Lj 029A eopenclosed 01BA ezhtail 0318 lefttackbelowcmb 030A ringcmb 01BB twostroke 029B Gsmallhook 030B hungarumlautcmb 01D0 icaron 01C9 lj 0320 minusbelowcmb 0319 righttackbelowcmb 01BC Tonefive 01D1 Ocaron 030C caroncmb 0321 hookpalatalizedbelowcmb 030D verticallineabovecmb 01BD tonefive 0322 hookretroflexbelowcmb 029D jcrossedtail 01D2 ocaron 01D3 Ucaron 030E dblverticallineabovecmb 0323 dotbelowcomb 01BE glottalinvertedstroke 029E kturned 01BF wynn 01D4 ucaron 030F dblgravecmb 0324 dieresisbelowcmb 01D5 Udieresismacron 0325 ringbelowcmb 01D6 udieresismacron 01D7 Udieresisacute 0327 cedillacmb 01D8 udieresisacute 01CA NJ 031A leftangleabovecmb 0328 ogonekcmb 0329 verticallinebelowcmb 0330 tildebelowcmb 01D9 Udieresiscaron 01CB Nj 01E0 Adotmacron 031B horncmb F73F questionsmall 01E1 adotmacron 0331 macronbelowcmb 01CC nj 031C ringhalfleftbelowcmb 031D uptackbelowcmb 01CD Acaron 01E2 AEmacron 0332 lowlinecmb 01E3 aemacron 01CE acaron 0333 dbllowlinecmb 031E downtackbelowcmb 0334 tildeoverlaycmb 01CF Icaron 01E4 Gstroke 031F plusbelowcmb 0335 strokeshortoverlaycmb F760 Gravesmall 01E5 gstroke 0336 strokelongoverlaycmb 01E6 Gcaron F761 Asmall 0337 solidusshortoverlaycmb F762 Bsmall 01E7 gcaron 01DA udieresiscaron 0338 soliduslongoverlaycmb 01E8 Kcaron F763 Csmall 032A bridgebelowcmb 01DB Udieresisgrave F764 Dsmall 032B dblarchinvertedbelowcmb 0340 gravetonecmb 01F0 jcaron 01E9 kcaron 0339 ringhalfrightbelowcmb 01DC udieresisgrave 0341 acutetonecmb 01F1 DZ F765 Esmall 032C caronbelowcmb F766 Fsmall 01F2 Dz 032D circumflexbelowcmb 01DD eturned 0342 perispomenigreekcmb F767 Gsmall 01DE Adieresismacron 032E brevebelowcmb 01F3 dz 0343 koroniscmb 01DF adieresismacron F768 Hsmall 01F4 Gacute 032F breveinvertedbelowcmb 0344 dialytikatonoscmb 0345 ypogegrammenigreekcmb F770 Psmall F769 Ismall 01F5 gacute F771 Qsmall F772 Rsmall F773 Ssmall 01EA Oogonek 033A bridgeinvertedbelowcmb 033B squarebelowcmb F774 Tsmall 01EB oogonek F775 Usmall 01EC Oogonekmacron 033C seagullbelowcmb 033D xabovecmb F776 Vsmall 01ED oogonekmacron 033E tildeverticalcmb F777 Wsmall 01EE Ezhcaron F778 Xsmall F76A Jsmall 033F dbloverlinecmb 01EF ezhcaron F779 Ysmall F76B Ksmall F76C Lsmall F76D Msmall F76E Nsmall 01FA Aringacute 0360 tildedoublecmb F76F Osmall 01FB aringacute 01FC AEacute 0361 breveinverteddoublecmb 01FD aeacute 01FE Ostrokeacute F77A Zsmall 01FF ostrokeacute 0374 numeralsigngreek F6C0 ll 0375 numeralsignlowergreek F6C3 commaaccent F6C4 afii10063 02A0 qhook F6C5 afii10064 02A1 glottalstopstroke F6C6 afii10192 02A2 glottalstopstrokereversed F6C7 afii10831 02A3 dzaltone 0384 tonos F6C8 afii10832 02A4 dezh F6D0 Macron F6C9 Acute 0385 dieresistonos 02A5 dzcurl 02A6 ts 0386 Alphatonos F6D1 cyrBreve 02A7 tesh 0387 anoteleia F6D2 cyrFlex 037A ypogegrammeni 02A8 tccurl 0388 Epsilontonos F6D3 dblGrave F6BE dotlessj F6BF LL 0389 Etatonos F6D4 cyrbreve 02B0 hsuperior 0390 iotadieresistonos 0401 afii10023 0391 Alpha F6D5 cyrflex 02B1 hhooksuperior 0392 Beta 0402 afii10051 F6D6 dblgrave 02B2 jsuperior 0403 afii10052 0393 Gamma F6D7 dieresisacute 037E questiongreek F6CA Caron 0394 Deltagreek 0404 afii10053 F6D8 dieresisgrave 02B4 rturnedsuperior F6CB Dieresis 0405 afii10054 0395 Epsilon F6E0 centsuperior F6D9 copyrightserif 02B5 rhookturnedsuperior 0396 Zeta 02B6 Rsmallinvertedsuperior 0406 afii10055 F6CC DieresisAcute F6E1 commainferior 02B7 wsuperior 0407 afii10056 F6CD DieresisGrave 0397 Eta F6E2 commasuperior 02B8 ysuperior 0398 Theta 0408 afii10057 038A Iotatonos F6CE Grave F6E3 dollarinferior 0409 afii10058 0399 Iota F6CF Hungarumlaut 0410 afii10017 F6E4 dollarsuperior 02C0 glottalstopmod 02B9 primemod 038C Omicrontonos 0411 afii10018 02C1 glottalstopreversedmod F6E5 hypheninferior 0412 afii10019 02C2 arrowheadleftmod F6E6 hyphensuperior 038E Upsilontonos 0413 afii10020 02C3 arrowheadrightmod F6E7 periodinferior 038F Omegatonos 0414 afii10021 02C4 arrowheadupmod F6E8 periodsuperior F6DA registerserif F6DB trademarkserif 0415 afii10022 02C5 arrowheaddownmod F6E9 asuperior F6F0 osuperior 0416 afii10024 02C6 circumflex F6DC onefitted F6F1 rsuperior F6F2 ssuperior 0417 afii10025 02C7 caron F6DD rupiah 02C8 verticallinemod F6F3 tsuperior F6DE threequartersemdash 040A afii10059 039A Kappa 0418 afii10026 02BA dblprimemod 040B afii10060 039B Lambda 0419 afii10027 F6F4 Brevesmall 0420 afii10034 F6DF centinferior 02D0 colontriangularmod 02BB commaturnedmod 02C9 firsttonechinese 039C Mu 040C afii10061 F6F5 Caronsmall 0421 afii10035 02BC apostrophemod 02D1 colontriangularhalfmod 0422 afii10036 039D Nu F6F6 Circumflexsmall 02BD commareversedmod 02D2 ringhalfrightcentered 039E Xi 040E afii10062 0423 afii10037 F6F7 Dotaccentsmall 02D3 ringhalfleftcentered 02BE ringhalfright 02D4 uptackmod 039F Omicron F6F8 Hungarumlautsmall 040F afii10145 0424 afii10038 F6EA bsuperior 02BF ringhalfleft F6F9 Lslashsmall 0425 afii10039 02D5 downtackmod F6EB dsuperior 0426 afii10040 F6EC esuperior 02D6 plusmod 0427 afii10041 F6ED isuperior 02D7 minusmod 0428 afii10042 041A afii10028 02D8 breve F6EE lsuperior 02CA secondtonechinese 0430 afii10065 0429 afii10043 041B afii10029 02D9 dotaccent 02CB fourthtonechinese 02E0 gammasuperior F6EF msuperior 02CC verticallinelowmod 041C afii10030 0431 becyrillic 041D afii10031 0432 vecyrillic 02CD macronlowmod 02E3 xsuperior 041E afii10032 0433 gecyrillic 02CE gravelowmod 02CF acutelowmod 041F afii10033 F6FA OEsmall 0434 decyrillic 02E4 glottalstopreversedsuperior 02E5 tonebarextrahighmod F6FB Ogoneksmall 0435 iecyrillic 02E6 tonebarhighmod F6FC Ringsmall 0436 zhecyrillic 02E7 tonebarmidmod F6FD Scaronsmall 0437 zecyrillic 02E8 tonebarlowmod F6FE Tildesmall 042A afii10044 0438 iicyrillic 02DA ring 02E9 tonebarextralowmod F6FF Zcaronsmall 042B afii10045 0439 iishortcyrillic 0440 ercyrillic 02DB ogonek 042C afii10046 0441 escyrillic 02DC tilde 042D afii10047 0442 tecyrillic 02DD hungarumlaut 042E afii10048 0443 ucyrillic 02DE rhotichookmod 042F afii10049 0444 efcyrillic 0445 khacyrillic 0446 tsecyrillic 0447 checyrillic 043A kacyrillic 0448 shacyrillic 043B elcyrillic 0449 shchacyrillic 0451 iocyrillic 043C emcyrillic 043D encyrillic 0452 djecyrillic 043E ocyrillic 0453 gjecyrillic 043F pecyrillic 0454 ecyrillic 0455 dzecyrillic 0456 icyrillic F7A1 exclamdownsmall 0457 yicyrillic F7A2 centoldstyle 044A hardsigncyrillic 0458 jecyrillic 0460 Omegacyrillic 044B yericyrillic 0459 ljecyrillic F884 maihanakatleftthai 044C softsigncyrillic 0461 omegacyrillic F885 saraileftthai 0462 afii10146 044D ereversedcyrillic F886 saraiileftthai 044E iucyrillic 0463 yatcyrillic F887 saraueleftthai F7A8 Dieresissmall 0464 Eiotifiedcyrillic 044F iacyrillic F888 saraueeleftthai 0465 eiotifiedcyrillic F889 maitaikhuleftthai F890 maitriupperleftthai 0466 Yuslittlecyrillic F891 maitrilowrightthai 0467 yuslittlecyrillic F892 maitrilowleftthai 0468 Yuslittleiotifiedcyrillic 045A njecyrillic F893 maichattawaupperleftthai 0469 yuslittleiotifiedcyrillic 0470 Psicyrillic F7B4 Acutesmall 045B tshecyrillic F894 maichattawalowrightthai 045C kjecyrillic F895 maichattawalowleftthai 0471 psicyrillic F896 thanthakhatupperleftthai 0472 afii10147 F897 thanthakhatlowrightthai 045E ushortcyrillic 0473 fitacyrillic F898 thanthakhatlowleftthai 0474 afii10148 F7B8 Cedillasmall 045F dzhecyrillic F88A maiekupperleftthai 0475 izhitsacyrillic F88B maieklowrightthai F899 nikhahitleftthai 0476 Izhitsadblgravecyrillic F88C maieklowleftthai 0477 izhitsadblgravecyrillic F88D maithoupperleftthai 046A Yusbigcyrillic 0478 Ukcyrillic F88E maitholowrightthai 046B yusbigcyrillic 0479 ukcyrillic 03A0 Pi F7AF Macronsmall 0480 Koppacyrillic F88F maitholowleftthai 046C Yusbigiotifiedcyrillic 03A1 Rho 0481 koppacyrillic 046D yusbigiotifiedcyrillic 0482 thousandcyrillic 0483 titlocyrilliccmb 03A3 Sigma 046E Ksicyrillic 03A4 Tau 046F ksicyrillic 0484 palatalizationcyrilliccmb 03A5 Upsilon 0485 dasiapneumatacyrilliccmb 03A6 Phi 0486 psilipneumatacyrilliccmb 03A7 Chi 03A8 Psi 047A Omegaroundcyrillic 03B0 upsilondieresistonos 03A9 Omegagreek 0490 afii10050 047B omegaroundcyrillic F7BF questiondownsmall 047C Omegatitlocyrillic 0491 gheupturncyrillic 03B1 alpha 0492 Ghestrokecyrillic 03B2 beta 047D omegatitlocyrillic 047E Otcyrillic 03B3 gamma 0493 ghestrokecyrillic 0494 Ghemiddlehookcyrillic 03B4 delta 047F otcyrillic F7E0 Agravesmall 03B5 epsilon 0495 ghemiddlehookcyrillic 03B6 zeta 0496 Zhedescendercyrillic F7E1 Aacutesmall 0497 zhedescendercyrillic F7E2 Acircumflexsmall 03B7 eta 03B8 theta 0498 Zedescendercyrillic 03AA Iotadieresis F7E3 Atildesmall 0499 zedescendercyrillic 03AB Upsilondieresis F7E4 Adieresissmall 03B9 iota 03C0 pi F7E5 Aringsmall 03AC alphatonos 03C1 rho 03C2 sigmafinal F7E6 AEsmall 03AD epsilontonos 03C3 sigma F7E7 Ccedillasmall 03AE etatonos 03C4 tau F7E8 Egravesmall 03AF iotatonos 03C5 upsilon F7E9 Eacutesmall F7F0 Ethsmall F7F1 Ntildesmall 03C6 phi F7F2 Ogravesmall 03C7 chi F7F3 Oacutesmall 049A Kadescendercyrillic 03BA kappa 03C8 psi F7F4 Ocircumflexsmall 03D0 betasymbolgreek 049B kadescendercyrillic 03BB lambda 03C9 omega 03D1 thetasymbolgreek F7F5 Otildesmall 049C Kaverticalstrokecyrillic 03BC mugreek 03D2 Upsilonhooksymbol F7F6 Odieresissmall 049D kaverticalstrokecyrillic 03BD nu 03BE xi 03D3 Upsilonacutehooksymbolgreek 049E Kastrokecyrillic 03D4 Upsilondieresishooksymbolgreek F7F8 Oslashsmall F7EA Ecircumflexsmall 049F kastrokecyrillic 03BF omicron F7F9 Ugravesmall F7EB Edieresissmall 03D5 phisymbolgreek F7EC Igravesmall 03D6 pisymbolgreek F7ED Iacutesmall F7EE Icircumflexsmall 03CA iotadieresis 03CB upsilondieresis 03E0 Sampigreek F7EF Idieresissmall 0531 Aybarmenian 03CC omicrontonos 03CD upsilontonos 03E2 Sheicoptic 0532 Benarmenian 0533 Gimarmenian 03CE omegatonos 03E3 sheicoptic F7FA Uacutesmall 03E4 Feicoptic 0534 Daarmenian F7FB Ucircumflexsmall 0535 Echarmenian 03E5 feicoptic 0536 Zaarmenian F7FC Udieresissmall 03E6 Kheicoptic F7FD Yacutesmall 0537 Eharmenian 03E7 kheicoptic F7FE Thornsmall 03DA Stigmagreek 03E8 Horicoptic 0538 Etarmenian F7FF Ydieresissmall 0539 Toarmenian 0540 Hoarmenian 03E9 horicoptic 03F0 kappasymbolgreek 0541 Jaarmenian 03DC Digammagreek 03F1 rhosymbolgreek 03F2 sigmalunatesymbolgreek 0542 Ghadarmenian 03F3 yotgreek 03DE Koppagreek 0543 Cheharmenian 0544 Menarmenian 0545 Yiarmenian 0546 Nowarmenian 0547 Shaarmenian 053A Zhearmenian 0548 Voarmenian 03EA Gangiacoptic 0550 Reharmenian 053B Iniarmenian 0549 Chaarmenian 03EB gangiacoptic 03EC Shimacoptic 053C Liwnarmenian 0551 Coarmenian 03ED shimacoptic 0552 Yiwnarmenian 053D Xeharmenian 0553 Piwrarmenian 053E Caarmenian 03EE Deicoptic 053F Kenarmenian 0554 Keharmenian 03EF deicoptic 0555 Oharmenian 0556 Feharmenian 054A Peharmenian 054B Jheharmenian 0559 ringhalfleftarmenian 054C Raarmenian 0561 aybarmenian 054D Seharmenian 0562 benarmenian 054E Vewarmenian 0563 gimarmenian 054F Tiwnarmenian 0564 daarmenian 0565 echarmenian 0566 zaarmenian 0567 eharmenian 055A apostrophearmenian 0568 etarmenian 0569 toarmenian 055B emphasismarkarmenian 0570 hoarmenian 055C exclamarmenian 0571 jaarmenian 055D commaarmenian 0572 ghadarmenian 0573 cheharmenian 055E questionarmenian 055F abbreviationmarkarmenian 0574 menarmenian 0575 yiarmenian 0576 nowarmenian 0577 shaarmenian 056A zhearmenian 0578 voarmenian 04A0 Kabashkircyrillic 0579 chaarmenian 056B iniarmenian 0580 reharmenian 0581 coarmenian 04A1 kabashkircyrillic 056C liwnarmenian 0582 yiwnarmenian 056D xeharmenian 04A2 Endescendercyrillic 056E caarmenian 04A3 endescendercyrillic 0583 piwrarmenian 04A4 Enghecyrillic 0584 keharmenian 056F kenarmenian 04A5 enghecyrillic 0585 oharmenian 04A6 Pemiddlehookcyrillic 0586 feharmenian 0587 echyiwnarmenian 04A7 pemiddlehookcyrillic 04A8 Haabkhasiancyrillic 057A peharmenian 04B0 Ustraightstrokecyrillic 04A9 haabkhasiancyrillic 057B jheharmenian 0589 periodarmenian 04B1 ustraightstrokecyrillic 0591 etnahtalefthebrew 057C raarmenian 04B2 Hadescendercyrillic 0592 segoltahebrew 057D seharmenian 057E vewarmenian 04B3 hadescendercyrillic 0593 shalshelethebrew 0594 zaqefqatanhebrew 057F tiwnarmenian 04B4 Tetsecyrillic 0595 zaqefgadolhebrew 04B5 tetsecyrillic 0596 tipehalefthebrew 04B6 Chedescendercyrillic 04B7 chedescendercyrillic 0597 reviamugrashhebrew 0598 zarqahebrew 04B8 Cheverticalstrokecyrillic 04AA Esdescendercyrillic 04B9 cheverticalstrokecyrillic 04AB esdescendercyrillic 04C0 palochkacyrillic 0599 pashtahebrew 04C1 Zhebrevecyrillic 04AC Tedescendercyrillic F8E5 radicalex 04C2 zhebrevecyrillic 04AD tedescendercyrillic F8E6 arrowvertex 04AE Ustraightcyrillic 04C3 Kahookcyrillic F8E7 arrowhorizex 04AF ustraightcyrillic 04C4 kahookcyrillic F8E8 registersans F8F0 bracketleftbt F8E9 copyrightsans F8F1 bracelefttp 04C7 Enhookcyrillic F8F2 braceleftmid 059A yetivhebrew 04BA Shhacyrillic F8F3 braceleftbt 04C8 enhookcyrillic 059B tevirlefthebrew 04BB shhacyrillic 04D0 Abrevecyrillic F8F4 braceex 04D1 abrevecyrillic 04BC Cheabkhasiancyrillic 060C commaarabic 0621 hamzalowarabic 059C gereshaccenthebrew F8F5 integralex 04D2 Adieresiscyrillic 0622 alefmaddaabovearabic 04BD cheabkhasiancyrillic 059D gereshmuqdamhebrew F8F6 parenrighttp 04D3 adieresiscyrillic 04BE Chedescenderabkhasiancyrillic 0623 alefhamzaabovearabic 059E gershayimaccenthebrew F8F7 parenrightex F8EA trademarksans 04D4 Aiecyrillic 0624 wawhamzaabovearabic 04BF chedescenderabkhasiancyrillic F8F8 parenrightbt 059F qarneyparahebrew 0625 alefhamzabelowarabic 04D5 aiecyrillic F8F9 bracketrighttp F8EB parenlefttp 04D6 Iebrevecyrillic 0626 yehhamzaabovearabic F8EC parenleftex 0627 alefarabic 04D7 iebrevecyrillic F8ED parenleftbt 04D8 Schwacyrillic 0628 beharabic F8EE bracketlefttp 04CB Chekhakassiancyrillic 04E0 Dzeabkhasiancyrillic 04D9 schwacyrillic 061B semicolonarabic 0629 tehmarbutaarabic 0630 thalarabic F8EF bracketleftex 0631 reharabic 04CC chekhakassiancyrillic 04E1 dzeabkhasiancyrillic 04E2 Imacroncyrillic 0632 zainarabic 0633 seenarabic 04E3 imacroncyrillic 04E4 Idieresiscyrillic 061F questionarabic 0634 sheenarabic F8FA bracketrightex 0635 sadarabic F8FB bracketrightbt 04E5 idieresiscyrillic 04E6 Odieresiscyrillic 0636 dadarabic F8FC bracerighttp 0637 taharabic F8FD bracerightmid 04E7 odieresiscyrillic 04DA Schwadieresiscyrillic 04E8 Obarredcyrillic 062A teharabic 0638 zaharabic F8FE bracerightbt 04F0 Udieresiscyrillic 062B theharabic 0639 ainarabic 0640 tatweelarabic F8FF apple 04E9 obarredcyrillic 04DB schwadieresiscyrillic 04F1 udieresiscyrillic 04DC Zhedieresiscyrillic 062C jeemarabic 0641 feharabic 04DD zhedieresiscyrillic 04F2 Uhungarumlautcyrillic 062D haharabic 0642 qafarabic 04F3 uhungarumlautcyrillic 04DE Zedieresiscyrillic 062E khaharabic 0643 kafarabic 04DF zedieresiscyrillic 04F4 Chedieresiscyrillic 062F dalarabic 0644 lamarabic 0645 meemarabic 04F5 chedieresiscyrillic 0646 noonarabic 0647 heharabic 04F8 Yerudieresiscyrillic 04EA Obarreddieresiscyrillic 063A ghainarabic 0648 wawarabic 04F9 yerudieresiscyrillic 0649 alefmaksuraarabic 0650 kasraarabic 04EB obarreddieresiscyrillic 0651 shaddaarabic 0652 sukunarabic 04EE Umacroncyrillic 04EF umacroncyrillic 064A yeharabic 0660 zerohackarabic 064B fathatanarabic 0661 onehackarabic 064C dammatanarabic 0662 twohackarabic 064D kasratanarabic 0663 threehackarabic 064E fathalowarabic 0664 fourhackarabic 064F dammalowarabic 0665 fivehackarabic 0666 sixhackarabic 0667 sevenhackarabic 0668 eighthackarabic 0669 ninehackarabic 066A percentarabic 05A0 telishagedolahebrew 0679 tteharabic 066B decimalseparatorpersian 066C thousandsseparatorpersian 05A1 pazerhebrew 066D asteriskarabic 05A3 munahlefthebrew 05A4 mahapakhlefthebrew 05A5 merkhalefthebrew 0686 tcheharabic 05A6 merkhakefulalefthebrew 05A7 dargalefthebrew 0688 ddalarabic 05A8 qadmahebrew 05A9 telishaqetanahebrew 05B0 shevawidehebrew 0691 rreharabic 05B1 hatafsegolwidehebrew 05B2 hatafpatahwidehebrew 067E peharabic 05B3 hatafqamatswidehebrew 05B4 hiriqwidehebrew 05B5 tserewidehebrew 05B6 segolwidehebrew 05B7 patahwidehebrew 05AA yerahbenyomolefthebrew 0698 jeharabic 05B8 qamatswidehebrew 05B9 holamwidehebrew 05C0 paseqhebrew 05AB olehebrew 05C1 shindothebrew 05AC iluyhebrew 05C2 sindothebrew 05AD dehihebrew 05AE zinorhebrew 05C3 sofpasuqhebrew {05D3 05B0} daletshevahebrew 05C4 upperdothebrew {05D3 05B1} dalethatafsegolhebrew 05AF masoracirclehebrew {05D3 05B2} dalethatafpatahhebrew {05D3 05B4} dalethiriqhebrew {05D3 05B5} dalettserehebrew 05D0 alefhebrew 05BB qubutswidehebrew {05D3 05B6} daletsegolhebrew 05D1 bethebrew 05BC dageshhebrew {05D3 05B7} daletpatahhebrew 05D2 gimelhebrew 05BD siluqlefthebrew {05D3 05B8} daletqamatshebrew 05BE maqafhebrew 05D3 dalethebrew {05D3 05B9} daletholamhebrew 05D4 hehebrew 05BF rafehebrew 05D5 vavhebrew 05D6 zayinhebrew 05D7 hethebrew 05D8 tethebrew 05D9 yodhebrew 05E0 nunhebrew 05E1 samekhhebrew 05E2 ayinhebrew 05E3 finalpehebrew {05D3 05BB} daletqubutshebrew 05E4 pehebrew 05E5 finaltsadihebrew 05E6 tsadihebrew 05E7 qofhebrew 05DA finalkafhebrew 05E8 reshhebrew 05DB kafhebrew 05E9 shinhebrew 05F0 vavvavhebrew 05DC lamedhebrew 05F1 vavyodhebrew 05DD finalmemhebrew 05F2 yodyodhebrew 05DE memhebrew 05F3 gereshhebrew 05DF finalnunhebrew 05F4 gershayimhebrew 05EA tavhebrew 06A4 veharabic 06C1 hehaltonearabic 06AF gafarabic 06BA noonghunnaarabic 06D1 yehthreedotsbelowarabic 06D2 yehbarreearabic 06D5 afii57534 06F0 zeropersian 06F1 onepersian 06F2 twopersian 06F3 threepersian 06F4 fourpersian 06F5 fivepersian 06F6 sixpersian 06F7 sevenpersian 06F8 eightpersian 06F9 ninepersian 0901 candrabindudeva 0902 anusvaradeva 0903 visargadeva 0905 adeva 0906 aadeva 0907 ideva 0908 iideva 0909 udeva 0910 aideva 0911 ocandradeva 0912 oshortdeva 0913 odeva 0914 audeva 0915 kadeva 0916 khadeva 0917 gadeva 090A uudeva 0918 ghadeva 0920 tthadeva 0919 ngadeva 090B rvocalicdeva 0921 ddadeva 090C lvocalicdeva 0922 ddhadeva 090D ecandradeva 090E eshortdeva 0923 nnadeva 0924 tadeva 090F edeva 0925 thadeva 0926 dadeva 0927 dhadeva 091A cadeva 0928 nadeva 091B chadeva 0929 nnnadeva 0930 radeva 091C jadeva 0931 rradeva 091D jhadeva 0932 ladeva 0933 lladeva 091E nyadeva 091F ttadeva 0934 llladeva 0935 vadeva 0936 shadeva 0937 ssadeva 092A padeva 0938 sadeva 0939 hadeva 0940 iivowelsigndeva 092B phadeva 0941 uvowelsigndeva 092C badeva 0942 uuvowelsigndeva 092D bhadeva 092E madeva 0943 rvocalicvowelsigndeva 092F yadeva 0944 rrvocalicvowelsigndeva 0945 ecandravowelsigndeva 0946 eshortvowelsigndeva 0947 evowelsigndeva 0948 aivowelsigndeva 0949 ocandravowelsigndeva 0950 omdeva 0951 udattadeva 093C nuktadeva 0952 anudattadeva 093D avagrahadeva 093E aavowelsigndeva 0953 gravedeva 0954 acutedeva 093F ivowelsigndeva 094A oshortvowelsigndeva 0958 qadeva 0959 khhadeva 094B ovowelsigndeva 0960 rrvocalicdeva 094C auvowelsigndeva 0961 llvocalicdeva 094D viramadeva 0962 lvocalicvowelsigndeva 0963 llvocalicvowelsigndeva 0964 danda 0965 dbldanda 0966 zerodeva 0967 onedeva 0968 twodeva 095A ghhadeva 095B zadeva 0969 threedeva 0970 abbreviationsigndeva 095C dddhadeva 095D rhadeva 095E fadeva 095F yyadeva 096A fourdeva 096B fivedeva 096C sixdeva 0981 candrabindubengali 0982 anusvarabengali 096D sevendeva 0983 visargabengali 096E eightdeva 096F ninedeva 0985 abengali 0986 aabengali 0987 ibengali 0988 iibengali 0989 ubengali 0990 aibengali 0993 obengali 0994 aubengali 0995 kabengali 0996 khabengali 0997 gabengali 098A uubengali 0998 ghabengali 0999 ngabengali 098B rvocalicbengali 098C lvocalicbengali 098F ebengali 099A cabengali 099B chabengali 099C jabengali 099D jhabengali 099E nyabengali 099F ttabengali {0631 FEF3 FE8E 0644} rehyehaleflamarabic 09A0 tthabengali 09A1 ddabengali 09A2 ddhabengali 09A3 nnabengali 09A4 tabengali 09A5 thabengali 09A6 dabengali 09A7 dhabengali 09A8 nabengali 09B0 rabengali 09B2 labengali 09B6 shabengali 09B7 ssabengali 09AA pabengali 09B8 sabengali 09B9 habengali 09C0 iivowelsignbengali 09AB phabengali 09C1 uvowelsignbengali 09AC babengali 09C2 uuvowelsignbengali 09AD bhabengali 09AE mabengali 09C3 rvocalicvowelsignbengali 09AF yabengali 09C4 rrvocalicvowelsignbengali 09C7 evowelsignbengali 09C8 aivowelsignbengali 09BC nuktabengali 09BE aavowelsignbengali 09BF ivowelsignbengali 09D7 aulengthmarkbengali 09CB ovowelsignbengali 09E0 rrvocalicbengali 09CC auvowelsignbengali 09E1 llvocalicbengali 09CD viramabengali 09E2 lvocalicvowelsignbengali 09E3 llvocalicvowelsignbengali 09E6 zerobengali 09E7 onebengali 09E8 twobengali 09E9 threebengali 09F0 ramiddlediagonalbengali 09F1 ralowerdiagonalbengali 09DC rrabengali 09DD rhabengali 09F2 rupeemarkbengali 09F3 rupeesignbengali 09DF yyabengali 09F4 onenumeratorbengali 09F5 twonumeratorbengali 09F6 threenumeratorbengali 09F7 fournumeratorbengali 09F8 denominatorminusonenumeratorbengali 09EA fourbengali 09F9 sixteencurrencydenominatorbengali 09EB fivebengali 09EC sixbengali 09ED sevenbengali 09EE eightbengali 09EF ninebengali 09FA issharbengali {FEE7 FEEC} noonhehinitialarabic FB00 ff FB01 fi FB02 fl FB03 ffi FB04 ffl FB20 ayinaltonehebrew FB30 alefdageshhebrew FB31 betdageshhebrew FB32 gimeldageshhebrew FB33 daletdageshhebrew FB1F yodyodpatahhebrew FB34 hedageshhebrew FB35 vavdageshhebrew FB36 zayindageshhebrew FB38 tetdageshhebrew FB2A shinshindothebrew FB39 yoddageshhebrew FB2B shinsindothebrew FB40 nundageshhebrew FB2C shindageshshindothebrew FB41 samekhdageshhebrew FB2D shindageshsindothebrew FB2E alefpatahhebrew FB43 pefinaldageshhebrew FB2F alefqamatshebrew FB44 pedageshhebrew FB46 tsadidageshhebrew FB47 qofdageshhebrew FB3A finalkafdageshhebrew FB48 reshdageshhebrew FB49 shindageshhebrew FB3B kafdageshhebrew FB3C lameddageshhebrew FB3E memdageshhebrew FB57 pehfinalarabic FB4A tavdageshhebrew FB58 pehinitialarabic FB4B vavholamhebrew FB59 pehmedialarabic FB4C betrafehebrew FB4D kafrafehebrew FB4E perafehebrew FB4F aleflamedhebrew FB67 ttehfinalarabic FB68 ttehinitialarabic FB69 ttehmedialarabic FB6B vehfinalarabic FB6C vehinitialarabic FB6D vehmedialarabic FB7B tchehfinalarabic FB89 ddalfinalarabic FB7C tchehinitialarabic FB7D tchehmedialarabic FB93 gaffinalarabic FB94 gafinitialarabic FB95 gafmedialarabic FC08 behmeemisolatedarabic FB8B jehfinalarabic FB8D rrehfinalarabic FC0B tehjeemisolatedarabic FC0C tehhahisolatedarabic FC0E tehmeemisolatedarabic FB9F noonghunnafinalarabic FC48 meemmeemisolatedarabic FC58 yehmeemisolatedarabic FC4B noonjeemisolatedarabic FC60 shaddafathaarabic FC61 shaddadammaarabic FC62 shaddakasraarabic FC4E noonmeemisolatedarabic FC73 tehnoonfinalarabic FC5E shaddadammatanarabic FC5F shaddakasratanarabic FC6D behnoonfinalarabic FBA4 hehhamzaaboveisolatedarabic FBA5 hehhamzaabovefinalarabic FBA7 hehfinalaltonearabic FBA8 hehinitialaltonearabic FBA9 hehmedialaltonearabic FC94 yehnoonfinalarabic FC8D noonnoonfinalarabic FBAF yehbarreefinalarabic FC9F behmeeminitialarabic FD3E parenleftaltonearabic FD3F parenrightaltonearabic FCA1 tehjeeminitialarabic FCA2 tehhahinitialarabic FCA4 tehmeeminitialarabic FD88 lammeemhahinitialarabic FCC9 lamjeeminitialarabic FCD1 meemmeeminitialarabic FCD2 noonjeeminitialarabic FCD5 noonmeeminitialarabic 0A02 bindigurmukhi FCCA lamhahinitialarabic FE30 twodotleadervertical 0A05 agurmukhi FCCB lamkhahinitialarabic 0A06 aagurmukhi FE31 emdashvertical FCCC lammeeminitialarabic FE32 endashvertical 0A07 igurmukhi FE33 underscorevertical 0A08 iigurmukhi FE34 wavyunderscorevertical 0A09 ugurmukhi 0A10 aigurmukhi FE35 parenleftvertical FE36 parenrightvertical FE37 braceleftvertical 0A13 oogurmukhi 0A14 augurmukhi FE38 bracerightvertical FE39 tortoiseshellbracketleftvertical FE40 anglebracketrightvertical 0A15 kagurmukhi FE41 cornerbracketleftvertical 0A16 khagurmukhi FCDD yehmeeminitialarabic FE42 cornerbracketrightvertical 0A17 gagurmukhi FE43 whitecornerbracketleftvertical 0A0A uugurmukhi 0A18 ghagurmukhi FE44 whitecornerbracketrightvertical 0A20 tthagurmukhi 0A19 ngagurmukhi 0A21 ddagurmukhi 0A22 ddhagurmukhi 0A23 nnagurmukhi FE3A tortoiseshellbracketrightvertical 0A24 tagurmukhi 0A0F eegurmukhi 0A25 thagurmukhi FE3B blacklenticularbracketleftvertical FE50 commasmall FE49 overlinedashed FE3C blacklenticularbracketrightvertical 0A26 dagurmukhi FE3D dblanglebracketleftvertical 0A27 dhagurmukhi FE52 periodsmall 0A1A cagurmukhi FE3E dblanglebracketrightvertical 0A28 nagurmukhi FE3F anglebracketleftvertical 0A1B chagurmukhi 0A30 ragurmukhi FE54 semicolonsmall FE55 colonsmall 0A1C jagurmukhi 0A1D jhagurmukhi 0A32 lagurmukhi 0A1E nyagurmukhi 0A1F ttagurmukhi FE4A overlinecenterline 0A35 vagurmukhi FE4B overlinewavy FE59 parenleftsmall FE61 asterisksmall FE4C overlinedblwavy 0A36 shagurmukhi FE4D lowlinedashed FE62 plussmall FE63 hyphensmall FE4E lowlinecenterline 0A2A pagurmukhi 0A38 sagurmukhi FE4F underscorewavy 0A39 hagurmukhi 0A40 iimatragurmukhi FE64 lesssmall 0A2B phagurmukhi 0A41 umatragurmukhi 0A2C bagurmukhi FE65 greatersmall 0A42 uumatragurmukhi 0A2D bhagurmukhi FE66 equalsmall 0A2E magurmukhi 0A2F yagurmukhi FE5A parenrightsmall FE5B braceleftsmall FE69 dollarsmall FE5C bracerightsmall FE5D tortoiseshellbracketleftsmall 0A47 eematragurmukhi FE5E tortoiseshellbracketrightsmall 0A48 aimatragurmukhi FE5F numbersignsmall 0A3C nuktagurmukhi 0A3E aamatragurmukhi 0A3F imatragurmukhi FE6A percentsmall FE6B atsmall FE82 alefmaddaabovefinalarabic FE84 alefhamzaabovefinalarabic 0A59 khhagurmukhi 0A4B oomatragurmukhi 0A4C aumatragurmukhi FE86 wawhamzaabovefinalarabic 0A4D halantgurmukhi FE88 alefhamzabelowfinalarabic FE90 behfinalarabic 0A66 zerogurmukhi FE91 behinitialarabic FF01 exclammonospace FE92 behmedialarabic 0A67 onegurmukhi FF02 quotedblmonospace 0A68 twogurmukhi 0A5A ghhagurmukhi FF03 numbersignmonospace 0A5B zagurmukhi 0A70 tippigurmukhi 0A69 threegurmukhi FE94 tehmarbutafinalarabic FF04 dollarmonospace 0A71 addakgurmukhi FF05 percentmonospace 0A5C rragurmukhi FE96 tehfinalarabic FF06 ampersandmonospace 0A72 irigurmukhi 0A73 uragurmukhi FE97 tehinitialarabic 0A5E fagurmukhi FF07 quotesinglemonospace FE8A yehhamzaabovefinalarabic FE98 tehmedialarabic 0A74 ekonkargurmukhi FF08 parenleftmonospace FF10 zeromonospace FE8B yehhamzaaboveinitialarabic FF09 parenrightmonospace FE8C yehhamzaabovemedialarabic FF11 onemonospace FF12 twomonospace FF13 threemonospace FE8E aleffinalarabic 0A6A fourgurmukhi 0A6B fivegurmukhi FF14 fourmonospace 0A6C sixgurmukhi 0A81 candrabindugujarati FF15 fivemonospace FF16 sixmonospace 0A82 anusvaragujarati 0A6D sevengurmukhi 0A83 visargagujarati 0A6E eightgurmukhi FF17 sevenmonospace FE9A thehfinalarabic FF0A asteriskmonospace FF18 eightmonospace 0A6F ninegurmukhi FE9B thehinitialarabic 0A85 agujarati FF20 atmonospace FF19 ninemonospace FF0B plusmonospace FE9C thehmedialarabic 0A86 aagujarati FF21 Amonospace FF0C commamonospace FF22 Bmonospace FF0D hyphenmonospace 0A87 igujarati FF23 Cmonospace 0A88 iigujarati FE9E jeemfinalarabic FF0E periodmonospace 0A89 ugujarati FF0F slashmonospace FF24 Dmonospace 0A90 aigujarati FE9F jeeminitialarabic FF25 Emonospace 0A91 ocandragujarati FF26 Fmonospace 2002 enspace FF27 Gmonospace 0A93 ogujarati FF28 Hmonospace 0A94 augujarati FF1A colonmonospace FF30 Pmonospace FF29 Imonospace 0A95 kagujarati FF1B semicolonmonospace FF31 Qmonospace 0A96 khagujarati FF1C lessmonospace FF32 Rmonospace FF1D equalmonospace 0A97 gagujarati 0A8A uugujarati FF33 Smonospace 0A98 ghagujarati FF1E greatermonospace FF34 Tmonospace 2010 hyphentwo 0A99 ngagujarati FF1F questionmonospace 0A8B rvocalicgujarati FF35 Umonospace FF36 Vmonospace 0A8D ecandragujarati 2012 figuredash FF37 Wmonospace 2013 endash FF38 Xmonospace FF2A Jmonospace 0A8F egujarati 2014 emdash 2015 horizontalbar FF39 Ymonospace FF2B Kmonospace FF40 gravemonospace FF2C Lmonospace FF41 amonospace 2016 dblverticalbar FF2D Mmonospace FF42 bmonospace 2017 underscoredbl FDF2 lamlamhehisolatedarabic FF2E Nmonospace 0A9A cagujarati FF43 cmonospace 2018 quoteleft 200B zerowidthspace FF2F Omonospace 0A9B chagujarati 2020 dagger FF44 dmonospace 2019 quoteright 200C zerowidthnonjoiner 2021 daggerdbl FF45 emonospace 0A9C jagujarati 200D afii301 2022 bullet FF46 fmonospace 0A9D jhagujarati 200E afii299 FF47 gmonospace 0A9E nyagujarati 0A9F ttagujarati FF3A Zmonospace 200F afii300 FF48 hmonospace 2024 onedotenleader 2025 twodotleader FF3B bracketleftmonospace FF49 imonospace FF50 pmonospace FF3C backslashmonospace 2026 ellipsis FF51 qmonospace FF3D bracketrightmonospace FF52 rmonospace FF53 smonospace FF3E asciicircummonospace 201A quotesinglbase FF3F underscoremonospace FF54 tmonospace 2030 perthousand 201B quotereversed FF55 umonospace 201C quotedblleft FF56 vmonospace 2032 minute 201D quotedblright FF57 wmonospace 201E quotedblbase 2033 second FF58 xmonospace FF4A jmonospace FDFA sallallahoualayhewasallamarabic FF59 ymonospace FF4B kmonospace 2035 primereversed FF4C lmonospace FF61 periodhalfwidth FF62 cornerbracketlefthalfwidth FF4D mmonospace FF63 cornerbracketrighthalfwidth FF4E nmonospace 2039 guilsinglleft FF64 ideographiccommaleft FF4F omonospace 202C afii61573 FF65 middledotkatakanahalfwidth FF66 wokatakanahalfwidth 202D afii61574 2042 asterism 202E afii61575 FF67 asmallkatakanahalfwidth FF5A zmonospace 2044 fraction FF68 ismallkatakanahalfwidth FF69 usmallkatakanahalfwidth FF5B braceleftmonospace FF70 katahiraprolongmarkhalfwidth FF71 akatakanahalfwidth FF5C barmonospace FF5D bracerightmonospace FF72 ikatakanahalfwidth FF73 ukatakanahalfwidth FF5E asciitildemonospace 203A guilsinglright FF74 ekatakanahalfwidth 203B referencemark 203C exclamdbl FF75 okatakanahalfwidth FF76 kakatakanahalfwidth FF77 kikatakanahalfwidth 203E overline FF6A esmallkatakanahalfwidth FF78 kukatakanahalfwidth FF80 takatakanahalfwidth FEA0 jeemmedialarabic FF79 kekatakanahalfwidth FF6B osmallkatakanahalfwidth FF6C yasmallkatakanahalfwidth FF81 tikatakanahalfwidth FF6D yusmallkatakanahalfwidth FF82 tukatakanahalfwidth FEA2 hahfinalarabic FF6E yosmallkatakanahalfwidth FF83 tekatakanahalfwidth FEA3 hahinitialarabic FF6F tusmallkatakanahalfwidth FF84 tokatakanahalfwidth FEA4 hahmedialarabic FF85 nakatakanahalfwidth FEA6 khahfinalarabic FF86 nikatakanahalfwidth FEA7 khahinitialarabic FF87 nukatakanahalfwidth FEA8 khahmedialarabic FF7A kokatakanahalfwidth FF88 nekatakanahalfwidth FEB0 zainfinalarabic FF90 mikatakanahalfwidth FF89 nokatakanahalfwidth FF7B sakatakanahalfwidth FF7C sikatakanahalfwidth FF91 mukatakanahalfwidth FF7D sukatakanahalfwidth FF92 mekatakanahalfwidth FEB2 seenfinalarabic FF93 mokatakanahalfwidth FEB3 seeninitialarabic FF7E sekatakanahalfwidth 2070 zerosuperior FF94 yakatakanahalfwidth FF7F sokatakanahalfwidth FEB4 seenmedialarabic FF95 yukatakanahalfwidth FF96 yokatakanahalfwidth FEB6 sheenfinalarabic FF97 rakatakanahalfwidth FEB7 sheeninitialarabic FEAA dalfinalarabic 2074 foursuperior FF8A hakatakanahalfwidth FF98 rikatakanahalfwidth FEB8 sheenmedialarabic FEC0 dadmedialarabic 2075 fivesuperior FF8B hikatakanahalfwidth FF99 rukatakanahalfwidth FEAC thalfinalarabic 2076 sixsuperior FF8C hukatakanahalfwidth FEC2 tahfinalarabic FF8D hekatakanahalfwidth 2077 sevensuperior FEC3 tahinitialarabic 2078 eightsuperior FF8E hokatakanahalfwidth FEAE rehfinalarabic 2080 zeroinferior 0AA0 tthagujarati FEC4 tahmedialarabic FF8F makatakanahalfwidth 2079 ninesuperior 0AA1 ddagujarati 2081 oneinferior FEC6 zahfinalarabic 2082 twoinferior 0AA2 ddhagujarati FEC7 zahinitialarabic 2083 threeinferior 0AA3 nnagujarati FEC8 zahmedialarabic 0AA4 tagujarati 2084 fourinferior FF9A rekatakanahalfwidth FEBA sadfinalarabic 0AA5 thagujarati 2085 fiveinferior FED0 ghainmedialarabic FF9B rokatakanahalfwidth FEBB sadinitialarabic FF9C wakatakanahalfwidth 2086 sixinferior 0AA6 dagujarati FEBC sadmedialarabic 0AA7 dhagujarati FED2 fehfinalarabic FF9D nkatakanahalfwidth 2087 seveninferior FF9E voicedmarkkanahalfwidth FEBE dadfinalarabic 2088 eightinferior FED3 fehinitialarabic 0AA8 nagujarati 207A plussuperior FEBF dadinitialarabic FED4 fehmedialarabic 2089 nineinferior 0AB0 ragujarati FF9F semivoicedmarkkanahalfwidth 207C equalsuperior 0AB2 lagujarati 207D parenleftsuperior FED6 qaffinalarabic 2103 centigrade 0AB3 llagujarati 207E parenrightsuperior FED7 qafinitialarabic FECA ainfinalarabic 207F nsuperior FED8 qafmedialarabic 0AB5 vagujarati 2105 careof FECB aininitialarabic FEE0 lammedialarabic FECC ainmedialarabic 0AB6 shagujarati 0AB7 ssagujarati FEE2 meemfinalarabic FECE ghainfinalarabic FEE3 meeminitialarabic 0AAA pagujarati 0AB8 sagujarati 2109 fahrenheit FECF ghaininitialarabic 0AB9 hagujarati 0AC0 iivowelsigngujarati FEE4 meemmedialarabic 0AAB phagujarati 0AC1 uvowelsigngujarati 2111 Ifraktur 0AAC bagujarati 0AC2 uuvowelsigngujarati 0AAD bhagujarati FEE6 noonfinalarabic 208D parenleftinferior 2113 lsquare 0AAE magujarati FEE7 nooninitialarabic 208E parenrightinferior 0AC3 rvocalicvowelsigngujarati 0AAF yagujarati FEDA kaffinalarabic FEE8 noonmedialarabic 0AC4 rrvocalicvowelsigngujarati FEF0 alefmaksurafinalarabic 0AC5 ecandravowelsigngujarati FEDB kafinitialarabic 2116 numero FEDC kafmedialarabic FEF2 yehfinalarabic 0AC7 evowelsigngujarati 2118 weierstrass 0AC8 aivowelsigngujarati FEF3 yehinitialarabic FEDE lamfinalarabic FEF4 yehmedialarabic FEDF laminitialarabic 0AC9 ocandravowelsigngujarati 0AD0 omgujarati 2121 telephone FEF5 lamalefmaddaaboveisolatedarabic 0ABC nuktagujarati 2122 trademark FEF6 lamalefmaddaabovefinalarabic 0ABE aavowelsigngujarati FEF7 lamalefhamzaaboveisolatedarabic FEEA hehfinalarabic 0ABF ivowelsigngujarati FEF8 lamalefhamzaabovefinalarabic FEEB hehinitialarabic FEF9 lamalefhamzabelowisolatedarabic 2126 Omega FEEC hehmedialarabic FEEE wawfinalarabic 0ACB ovowelsigngujarati 0AE0 rrvocalicgujarati 211C Rfraktur 0ACC auvowelsigngujarati 0ACD viramagujarati 211E prescription FEFA lamalefhamzabelowfinalarabic 2135 aleph FEFB lamalefisolatedarabic 0AE6 zerogujarati FEFC lamaleffinalarabic 0AE7 onegujarati 0AE8 twogujarati FEFF zerowidthjoiner 0AE9 threegujarati 212B angstrom 212E estimated 0AEA fourgujarati 0AEB fivegujarati 0AEC sixgujarati 0AED sevengujarati 0AEE eightgujarati 2153 onethird 2154 twothirds 0AEF ninegujarati 2160 Oneroman 2161 Tworoman 2162 Threeroman 2163 Fourroman 2164 Fiveroman 2165 Sixroman 2166 Sevenroman}
}

package provide pdf4tcl::metrics 0.3

# this file is auto-generated, please do NOT edit!

namespace eval pdf4tcl {
    variable font_widths
    variable font_metrics


set font_widths(AvantGarde-Book) {A 740 a 683 Aacute 740 aacute 683 Acircumflex 740 acircumflex 683 acute 375 Adieresis 740 adieresis 683 AE 992 ae 1157 Agrave 740 agrave 683 ampersand 757 Aring 740 aring 683 asciicircum 606 asciitilde 606 asterisk 425 at 867 Atilde 740 atilde 683 B 574 b 682 backslash 605 bar 672 braceleft 351 braceright 351 bracketleft 351 bracketright 351 breve 453 brokenbar 672 bullet 606 C 813 c 647 caron 502 Ccedilla 813 ccedilla 647 cedilla 324 cent 554 circumflex 502 colon 277 comma 277 copyright 747 currency 554 D 744 d 685 dagger 553 daggerdbl 553 degree 400 dieresis 369 divide 606 dollar 554 dotaccent 222 dotlessi 200 E 536 e 650 Eacute 536 eacute 650 Ecircumflex 536 ecircumflex 650 Edieresis 536 edieresis 650 Egrave 536 egrave 650 eight 554 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 790 eth 655 exclam 295 exclamdown 295 F 485 f 314 fi 487 five 554 fl 485 florin 554 four 554 fraction 166 G 872 g 673 germandbls 554 grave 378 greater 606 guillemotleft 425 guillemotright 425 guilsinglleft 251 guilsinglright 251 H 683 h 610 hungarumlaut 552 hyphen 332 I 226 i 200 Iacute 226 iacute 200 Icircumflex 226 icircumflex 200 Idieresis 226 idieresis 200 Igrave 226 igrave 200 J 482 j 203 K 591 k 502 L 462 l 200 less 606 logicalnot 606 Lslash 517 lslash 300 M 919 m 938 macron 485 minus 606 mu 608 multiply 606 N 740 n 610 nine 554 Ntilde 740 ntilde 610 numbersign 554 O 869 o 655 Oacute 869 oacute 655 Ocircumflex 869 ocircumflex 655 Odieresis 869 odieresis 655 OE 1194 oe 1137 ogonek 302 Ograve 869 ograve 655 one 554 onehalf 831 onequarter 831 onesuperior 332 ordfeminine 369 ordmasculine 369 Oslash 868 oslash 653 Otilde 869 otilde 655 P 592 p 682 paragraph 564 parenleft 369 parenright 369 percent 775 period 277 periodcentered 277 perthousand 1174 plus 606 plusminus 606 Q 871 q 682 question 591 questiondown 591 quotedbl 309 quotedblbase 502 quotedblleft 502 quotedblright 484 quoteleft 351 quoteright 351 quotesinglbase 354 quotesingle 198 R 607 r 301 registered 747 ring 332 S 498 s 388 Scaron 498 scaron 388 section 615 semicolon 277 seven 554 six 554 slash 437 space 277 sterling 554 T 426 t 339 Thorn 592 thorn 682 three 554 threequarters 831 threesuperior 332 tilde 439 trademark 1000 two 554 twosuperior 332 U 655 u 608 Uacute 655 uacute 608 Ucircumflex 655 ucircumflex 608 Udieresis 655 udieresis 608 Ugrave 655 ugrave 608 underscore 500 V 702 v 554 W 960 w 831 X 609 x 480 Y 592 y 536 Yacute 592 yacute 536 Ydieresis 592 ydieresis 536 yen 554 Z 480 z 425 Zcaron 480 zcaron 425 zero 554}
set font_widths(AvantGarde-BookOblique) {A 740 a 683 Aacute 740 aacute 683 Acircumflex 740 acircumflex 683 acute 375 Adieresis 740 adieresis 683 AE 992 ae 1157 Agrave 740 agrave 683 ampersand 757 Aring 740 aring 683 asciicircum 606 asciitilde 606 asterisk 425 at 867 Atilde 740 atilde 683 B 574 b 682 backslash 605 bar 672 braceleft 351 braceright 351 bracketleft 351 bracketright 351 breve 453 brokenbar 672 bullet 606 C 813 c 647 caron 502 Ccedilla 813 ccedilla 647 cedilla 324 cent 554 circumflex 502 colon 277 comma 277 copyright 747 currency 554 D 744 d 685 dagger 553 daggerdbl 553 degree 400 dieresis 369 divide 606 dollar 554 dotaccent 222 dotlessi 200 E 536 e 650 Eacute 536 eacute 650 Ecircumflex 536 ecircumflex 650 Edieresis 536 edieresis 650 Egrave 536 egrave 650 eight 554 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 790 eth 655 exclam 295 exclamdown 295 F 485 f 314 fi 487 five 554 fl 485 florin 554 four 554 fraction 166 G 872 g 673 germandbls 554 grave 378 greater 606 guillemotleft 425 guillemotright 425 guilsinglleft 251 guilsinglright 251 H 683 h 610 hungarumlaut 552 hyphen 332 I 226 i 200 Iacute 226 iacute 200 Icircumflex 226 icircumflex 200 Idieresis 226 idieresis 200 Igrave 226 igrave 200 J 482 j 203 K 591 k 502 L 462 l 200 less 606 logicalnot 606 Lslash 517 lslash 300 M 919 m 938 macron 485 minus 606 mu 608 multiply 606 N 740 n 610 nine 554 Ntilde 740 ntilde 610 numbersign 554 O 869 o 655 Oacute 869 oacute 655 Ocircumflex 869 ocircumflex 655 Odieresis 869 odieresis 655 OE 1194 oe 1137 ogonek 302 Ograve 869 ograve 655 one 554 onehalf 831 onequarter 831 onesuperior 332 ordfeminine 369 ordmasculine 369 Oslash 868 oslash 653 Otilde 869 otilde 655 P 592 p 682 paragraph 564 parenleft 369 parenright 369 percent 775 period 277 periodcentered 277 perthousand 1174 plus 606 plusminus 606 Q 871 q 682 question 591 questiondown 591 quotedbl 309 quotedblbase 502 quotedblleft 502 quotedblright 484 quoteleft 351 quoteright 351 quotesinglbase 354 quotesingle 198 R 607 r 301 registered 747 ring 332 S 498 s 388 Scaron 498 scaron 388 section 615 semicolon 277 seven 554 six 554 slash 437 space 277 sterling 554 T 426 t 339 Thorn 592 thorn 682 three 554 threequarters 831 threesuperior 332 tilde 439 trademark 1000 two 554 twosuperior 332 U 655 u 608 Uacute 655 uacute 608 Ucircumflex 655 ucircumflex 608 Udieresis 655 udieresis 608 Ugrave 655 ugrave 608 underscore 500 V 702 v 554 W 960 w 831 X 609 x 480 Y 592 y 536 Yacute 592 yacute 536 Ydieresis 592 ydieresis 536 yen 554 Z 480 z 425 Zcaron 480 zcaron 425 zero 554}
set font_widths(AvantGarde-Demi) {A 740 a 660 Aacute 740 aacute 660 Acircumflex 740 acircumflex 660 acute 420 Adieresis 740 adieresis 660 AE 900 ae 1080 Agrave 740 agrave 660 ampersand 680 Aring 740 aring 660 asciicircum 600 asciitilde 600 asterisk 440 at 740 Atilde 740 atilde 660 B 580 b 660 backslash 640 bar 600 braceleft 340 braceright 340 bracketleft 320 bracketright 320 breve 480 brokenbar 600 bullet 600 C 780 c 640 caron 540 Ccedilla 780 ccedilla 640 cedilla 340 cent 560 circumflex 540 colon 280 comma 280 copyright 740 currency 560 D 700 d 660 dagger 560 daggerdbl 560 degree 400 dieresis 500 divide 600 dollar 560 dotaccent 280 dotlessi 240 E 520 e 640 Eacute 520 eacute 640 Ecircumflex 520 ecircumflex 640 Edieresis 520 edieresis 640 Egrave 520 egrave 640 eight 560 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 742 eth 640 exclam 280 exclamdown 280 F 480 f 280 fi 520 five 560 fl 520 florin 560 four 560 fraction 160 G 840 g 660 germandbls 600 grave 420 greater 600 guillemotleft 460 guillemotright 460 guilsinglleft 240 guilsinglright 240 H 680 h 600 hungarumlaut 700 hyphen 420 I 280 i 240 Iacute 280 iacute 240 Icircumflex 280 icircumflex 240 Idieresis 280 idieresis 240 Igrave 280 igrave 240 J 480 j 260 K 620 k 580 L 440 l 240 less 600 logicalnot 600 Lslash 480 lslash 320 M 900 m 940 macron 420 minus 600 mu 576 multiply 600 N 740 n 600 nine 560 Ntilde 740 ntilde 600 numbersign 560 O 840 o 640 Oacute 840 oacute 640 Ocircumflex 840 ocircumflex 640 Odieresis 840 odieresis 640 OE 1060 oe 1080 ogonek 340 Ograve 840 ograve 640 one 560 onehalf 840 onequarter 840 onesuperior 336 ordfeminine 360 ordmasculine 360 Oslash 840 oslash 660 Otilde 840 otilde 640 P 560 p 660 paragraph 600 parenleft 380 parenright 380 percent 860 period 280 periodcentered 280 perthousand 1280 plus 600 plusminus 600 Q 840 q 660 question 560 questiondown 560 quotedbl 360 quotedblbase 480 quotedblleft 480 quotedblright 480 quoteleft 280 quoteright 280 quotesinglbase 280 quotesingle 220 R 580 r 320 registered 740 ring 360 S 520 s 440 Scaron 520 scaron 440 section 560 semicolon 280 seven 560 six 560 slash 460 space 280 sterling 560 T 420 t 300 Thorn 560 thorn 660 three 560 threequarters 840 threesuperior 336 tilde 480 trademark 1000 two 560 twosuperior 336 U 640 u 600 Uacute 640 uacute 600 Ucircumflex 640 ucircumflex 600 Udieresis 640 udieresis 600 Ugrave 640 ugrave 600 underscore 500 V 700 v 560 W 900 w 800 X 680 x 560 Y 620 y 580 Yacute 620 yacute 580 Ydieresis 620 ydieresis 580 yen 560 Z 500 z 460 Zcaron 500 zcaron 460 zero 560}
set font_widths(AvantGarde-DemiOblique) {A 740 a 660 Aacute 740 aacute 660 Acircumflex 740 acircumflex 660 acute 420 Adieresis 740 adieresis 660 AE 900 ae 1080 Agrave 740 agrave 660 ampersand 680 Aring 740 aring 660 asciicircum 600 asciitilde 600 asterisk 440 at 740 Atilde 740 atilde 660 B 580 b 660 backslash 640 bar 600 braceleft 340 braceright 340 bracketleft 320 bracketright 320 breve 480 brokenbar 600 bullet 600 C 780 c 640 caron 540 Ccedilla 780 ccedilla 640 cedilla 340 cent 560 circumflex 540 colon 280 comma 280 copyright 740 currency 560 D 700 d 660 dagger 560 daggerdbl 560 degree 400 dieresis 500 divide 600 dollar 560 dotaccent 280 dotlessi 240 E 520 e 640 Eacute 520 eacute 640 Ecircumflex 520 ecircumflex 640 Edieresis 520 edieresis 640 Egrave 520 egrave 640 eight 560 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 742 eth 640 exclam 280 exclamdown 280 F 480 f 280 fi 520 five 560 fl 520 florin 560 four 560 fraction 160 G 840 g 660 germandbls 600 grave 420 greater 600 guillemotleft 460 guillemotright 460 guilsinglleft 240 guilsinglright 240 H 680 h 600 hungarumlaut 700 hyphen 420 I 280 i 240 Iacute 280 iacute 240 Icircumflex 280 icircumflex 240 Idieresis 280 idieresis 240 Igrave 280 igrave 240 J 480 j 260 K 620 k 580 L 440 l 240 less 600 logicalnot 600 Lslash 480 lslash 320 M 900 m 940 macron 420 minus 600 mu 576 multiply 600 N 740 n 600 nine 560 Ntilde 740 ntilde 600 numbersign 560 O 840 o 640 Oacute 840 oacute 640 Ocircumflex 840 ocircumflex 640 Odieresis 840 odieresis 640 OE 1060 oe 1080 ogonek 340 Ograve 840 ograve 640 one 560 onehalf 840 onequarter 840 onesuperior 336 ordfeminine 360 ordmasculine 360 Oslash 840 oslash 660 Otilde 840 otilde 640 P 560 p 660 paragraph 600 parenleft 380 parenright 380 percent 860 period 280 periodcentered 280 perthousand 1280 plus 600 plusminus 600 Q 840 q 660 question 560 questiondown 560 quotedbl 360 quotedblbase 480 quotedblleft 480 quotedblright 480 quoteleft 280 quoteright 280 quotesinglbase 280 quotesingle 220 R 580 r 320 registered 740 ring 360 S 520 s 440 Scaron 520 scaron 440 section 560 semicolon 280 seven 560 six 560 slash 460 space 280 sterling 560 T 420 t 300 Thorn 560 thorn 660 three 560 threequarters 840 threesuperior 336 tilde 480 trademark 1000 two 560 twosuperior 336 U 640 u 600 Uacute 640 uacute 600 Ucircumflex 640 ucircumflex 600 Udieresis 640 udieresis 600 Ugrave 640 ugrave 600 underscore 500 V 700 v 560 W 900 w 800 X 680 x 560 Y 620 y 580 Yacute 620 yacute 580 Ydieresis 620 ydieresis 580 yen 560 Z 500 z 460 Zcaron 500 zcaron 460 zero 560}
set font_widths(Bookman-Demi) {A 720 a 580 Aacute 720 aacute 580 Acircumflex 720 acircumflex 580 acute 400 Adieresis 720 adieresis 580 AE 1140 ae 880 Agrave 720 agrave 580 ampersand 800 Aring 720 aring 580 asciicircum 600 asciitilde 600 asterisk 460 at 820 Atilde 720 atilde 580 B 720 b 600 backslash 600 bar 600 braceleft 320 braceright 320 bracketleft 300 bracketright 300 breve 500 brokenbar 600 bullet 460 C 740 c 580 caron 500 Ccedilla 740 ccedilla 580 cedilla 360 cent 660 circumflex 500 colon 340 comma 340 copyright 740 currency 660 D 780 d 640 dagger 440 daggerdbl 380 degree 400 dieresis 500 divide 600 dollar 660 dotaccent 320 dotlessi 360 E 720 e 580 Eacute 720 eacute 580 Ecircumflex 720 ecircumflex 580 Edieresis 720 edieresis 580 Egrave 720 egrave 580 eight 660 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 780 eth 620 exclam 360 exclamdown 360 F 680 f 380 fi 740 five 660 fl 740 florin 660 four 660 fraction 120 G 780 g 580 germandbls 660 grave 400 greater 600 guillemotleft 400 guillemotright 400 guilsinglleft 220 guilsinglright 220 H 820 h 680 hungarumlaut 440 hyphen 360 I 400 i 360 Iacute 400 iacute 360 Icircumflex 400 icircumflex 360 Idieresis 400 idieresis 360 Igrave 400 igrave 360 J 640 j 340 K 800 k 660 L 640 l 340 less 600 logicalnot 600 Lslash 640 lslash 340 M 940 m 1000 macron 460 minus 600 mu 660 multiply 600 N 740 n 680 nine 660 Ntilde 740 ntilde 680 numbersign 660 O 800 o 620 Oacute 800 oacute 620 Ocircumflex 800 ocircumflex 620 Odieresis 800 odieresis 620 OE 1220 oe 940 ogonek 320 Ograve 800 ograve 620 one 660 onehalf 990 onequarter 990 onesuperior 396 ordfeminine 400 ordmasculine 400 Oslash 800 oslash 620 Otilde 800 otilde 620 P 660 p 640 paragraph 800 parenleft 320 parenright 320 percent 940 period 340 periodcentered 340 perthousand 1360 plus 600 plusminus 600 Q 800 q 620 question 660 questiondown 660 quotedbl 420 quotedblbase 540 quotedblleft 540 quotedblright 540 quoteleft 320 quoteright 320 quotesinglbase 320 quotesingle 240 R 780 r 460 registered 740 ring 340 S 660 s 520 Scaron 660 scaron 520 section 600 semicolon 340 seven 660 six 660 slash 600 space 340 sterling 660 T 700 t 460 Thorn 660 thorn 640 three 660 threequarters 990 threesuperior 396 tilde 480 trademark 980 two 660 twosuperior 396 U 740 u 660 Uacute 740 uacute 660 Ucircumflex 740 ucircumflex 660 Udieresis 740 udieresis 660 Ugrave 740 ugrave 660 underscore 500 V 720 v 600 W 940 w 800 X 780 x 600 Y 700 y 620 Yacute 700 yacute 620 Ydieresis 700 ydieresis 620 yen 660 Z 640 z 560 Zcaron 640 zcaron 560 zero 660}
set font_widths(Bookman-DemiItalic) {A 720 a 680 Aacute 720 aacute 680 Acircumflex 720 acircumflex 680 acute 340 Adieresis 720 adieresis 680 AE 1140 ae 880 Agrave 720 agrave 680 ampersand 980 Aring 720 aring 680 asciicircum 620 asciitilde 620 asterisk 460 at 780 Atilde 720 atilde 680 B 720 b 600 backslash 580 bar 620 braceleft 300 braceright 300 bracketleft 260 bracketright 260 breve 460 brokenbar 620 bullet 360 C 700 c 560 caron 480 Ccedilla 700 ccedilla 560 cedilla 360 cent 680 circumflex 480 colon 340 comma 340 copyright 780 currency 680 D 760 d 680 dagger 420 daggerdbl 420 degree 400 dieresis 520 divide 600 dollar 680 dotaccent 380 dotlessi 380 E 720 e 560 Eacute 720 eacute 560 Ecircumflex 720 ecircumflex 560 Edieresis 720 edieresis 560 Egrave 720 egrave 560 eight 680 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 760 eth 600 exclam 320 exclamdown 320 F 660 f 420 fi 820 five 680 fl 820 florin 680 four 680 fraction 120 G 760 g 620 germandbls 660 grave 380 greater 620 guillemotleft 380 guillemotright 380 guilsinglleft 220 guilsinglright 220 H 800 h 700 hungarumlaut 560 hyphen 280 I 380 i 380 Iacute 380 iacute 380 Icircumflex 380 icircumflex 380 Idieresis 380 idieresis 380 Igrave 380 igrave 380 J 620 j 320 K 780 k 700 L 640 l 380 less 620 logicalnot 620 Lslash 640 lslash 380 M 860 m 960 macron 480 minus 600 mu 680 multiply 600 N 740 n 680 nine 680 Ntilde 740 ntilde 680 numbersign 680 O 760 o 600 Oacute 760 oacute 600 Ocircumflex 760 ocircumflex 600 Odieresis 760 odieresis 600 OE 1180 oe 920 ogonek 320 Ograve 760 ograve 600 one 680 onehalf 1020 onequarter 1020 onesuperior 408 ordfeminine 440 ordmasculine 440 Oslash 760 oslash 600 Otilde 760 otilde 600 P 640 p 660 paragraph 680 parenleft 260 parenright 260 percent 880 period 340 periodcentered 340 perthousand 1360 plus 600 plusminus 600 Q 760 q 620 question 620 questiondown 620 quotedbl 380 quotedblbase 520 quotedblleft 520 quotedblright 520 quoteleft 320 quoteright 320 quotesinglbase 300 quotesingle 180 R 740 r 500 registered 780 ring 360 S 700 s 540 Scaron 700 scaron 540 section 620 semicolon 340 seven 680 six 680 slash 360 space 340 sterling 680 T 700 t 440 Thorn 640 thorn 660 three 680 threequarters 1020 threesuperior 408 tilde 480 trademark 940 two 680 twosuperior 408 U 740 u 680 Uacute 740 uacute 680 Ucircumflex 740 ucircumflex 680 Udieresis 740 udieresis 680 Ugrave 740 ugrave 680 underscore 500 V 660 v 540 W 1000 w 860 X 740 x 620 Y 660 y 600 Yacute 660 yacute 600 Ydieresis 660 ydieresis 600 yen 680 Z 680 z 560 Zcaron 680 zcaron 560 zero 680}
set font_widths(Bookman-Light) {A 680 a 580 Aacute 680 aacute 580 Acircumflex 680 acircumflex 580 acute 340 Adieresis 680 adieresis 580 AE 1260 ae 860 Agrave 680 agrave 580 ampersand 800 Aring 680 aring 580 asciicircum 600 asciitilde 600 asterisk 440 at 820 Atilde 680 atilde 580 B 740 b 620 backslash 600 bar 600 braceleft 280 braceright 280 bracketleft 300 bracketright 300 breve 460 brokenbar 600 bullet 460 C 740 c 520 caron 420 Ccedilla 740 ccedilla 520 cedilla 320 cent 620 circumflex 420 colon 320 comma 320 copyright 740 currency 620 D 800 d 620 dagger 540 daggerdbl 540 degree 400 dieresis 420 divide 600 dollar 620 dotaccent 260 dotlessi 300 E 720 e 520 Eacute 720 eacute 520 Ecircumflex 720 ecircumflex 520 Edieresis 720 edieresis 520 Egrave 720 egrave 520 eight 620 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 800 eth 560 exclam 300 exclamdown 300 F 640 f 320 fi 620 five 620 fl 620 florin 620 four 620 fraction 140 G 800 g 540 germandbls 660 grave 340 greater 600 guillemotleft 360 guillemotright 360 guilsinglleft 240 guilsinglright 240 H 800 h 660 hungarumlaut 380 hyphen 400 I 340 i 300 Iacute 340 iacute 300 Icircumflex 340 icircumflex 300 Idieresis 340 idieresis 300 Igrave 340 igrave 300 J 600 j 300 K 720 k 620 L 600 l 300 less 600 logicalnot 600 Lslash 600 lslash 320 M 920 m 940 macron 440 minus 600 mu 680 multiply 600 N 740 n 660 nine 620 Ntilde 740 ntilde 660 numbersign 620 O 800 o 560 Oacute 800 oacute 560 Ocircumflex 800 ocircumflex 560 Odieresis 800 odieresis 560 OE 1240 oe 900 ogonek 320 Ograve 800 ograve 560 one 620 onehalf 930 onequarter 930 onesuperior 372 ordfeminine 420 ordmasculine 420 Oslash 800 oslash 560 Otilde 800 otilde 560 P 620 p 620 paragraph 600 parenleft 300 parenright 300 percent 900 period 320 periodcentered 320 perthousand 1280 plus 600 plusminus 600 Q 820 q 580 question 540 questiondown 540 quotedbl 380 quotedblbase 400 quotedblleft 400 quotedblright 400 quoteleft 220 quoteright 220 quotesinglbase 220 quotesingle 220 R 720 r 440 registered 740 ring 320 S 660 s 520 Scaron 660 scaron 520 section 520 semicolon 320 seven 620 six 620 slash 600 space 320 sterling 620 T 620 t 380 Thorn 620 thorn 620 three 620 threequarters 930 threesuperior 372 tilde 440 trademark 980 two 620 twosuperior 372 U 780 u 680 Uacute 780 uacute 680 Ucircumflex 780 ucircumflex 680 Udieresis 780 udieresis 680 Ugrave 780 ugrave 680 underscore 500 V 700 v 520 W 960 w 780 X 720 x 560 Y 640 y 540 Yacute 640 yacute 540 Ydieresis 640 ydieresis 540 yen 620 Z 640 z 480 Zcaron 640 zcaron 480 zero 620}
set font_widths(Bookman-LightItalic) {A 700 a 620 Aacute 700 aacute 620 Acircumflex 700 acircumflex 620 acute 320 Adieresis 700 adieresis 620 AE 1220 ae 880 Agrave 700 agrave 620 ampersand 820 Aring 700 aring 620 asciicircum 600 asciitilde 600 asterisk 440 at 780 Atilde 700 atilde 620 B 720 b 600 backslash 600 bar 600 braceleft 360 braceright 380 bracketleft 260 bracketright 260 breve 440 brokenbar 600 bullet 460 C 720 c 480 caron 440 Ccedilla 720 ccedilla 480 cedilla 320 cent 620 circumflex 440 colon 300 comma 300 copyright 740 currency 620 D 740 d 640 dagger 620 daggerdbl 620 degree 400 dieresis 420 divide 600 dollar 620 dotaccent 260 dotlessi 280 E 680 e 540 Eacute 680 eacute 540 Ecircumflex 680 ecircumflex 540 Edieresis 680 edieresis 540 Egrave 680 egrave 540 eight 620 ellipsis 1000 emdash 1000 endash 500 equal 600 Eth 740 eth 540 exclam 320 exclamdown 320 F 620 f 340 fi 640 five 620 fl 660 florin 620 four 620 fraction 20 G 760 g 560 germandbls 620 grave 340 greater 600 guillemotleft 300 guillemotright 300 guilsinglleft 180 guilsinglright 180 H 800 h 620 hungarumlaut 340 hyphen 320 I 320 i 280 Iacute 320 iacute 280 Icircumflex 320 icircumflex 280 Idieresis 320 idieresis 280 Igrave 320 igrave 280 J 560 j 280 K 720 k 600 L 580 l 280 less 600 logicalnot 600 Lslash 580 lslash 340 M 860 m 880 macron 440 minus 600 mu 620 multiply 600 N 720 n 620 nine 620 Ntilde 720 ntilde 620 numbersign 620 O 760 o 540 Oacute 760 oacute 540 Ocircumflex 760 ocircumflex 540 Odieresis 760 odieresis 540 OE 1180 oe 900 ogonek 260 Ograve 760 ograve 540 one 620 onehalf 930 onequarter 930 onesuperior 372 ordfeminine 440 ordmasculine 400 Oslash 760 oslash 540 Otilde 760 otilde 540 P 600 p 600 paragraph 620 parenleft 280 parenright 280 percent 800 period 300 periodcentered 300 perthousand 1180 plus 600 plusminus 600 Q 780 q 560 question 540 questiondown 540 quotedbl 360 quotedblbase 480 quotedblleft 440 quotedblright 440 quoteleft 280 quoteright 280 quotesinglbase 320 quotesingle 200 R 700 r 400 registered 740 ring 300 S 640 s 540 Scaron 640 scaron 540 section 620 semicolon 300 seven 620 six 620 slash 600 space 300 sterling 620 T 600 t 340 Thorn 600 thorn 600 three 620 threequarters 930 threesuperior 372 tilde 440 trademark 980 two 620 twosuperior 372 U 720 u 620 Uacute 720 uacute 620 Ucircumflex 720 ucircumflex 620 Udieresis 720 udieresis 620 Ugrave 720 ugrave 620 underscore 500 V 680 v 540 W 960 w 880 X 700 x 540 Y 660 y 600 Yacute 660 yacute 600 Ydieresis 660 ydieresis 600 yen 620 Z 580 z 520 Zcaron 580 zcaron 520 zero 620}
set font_widths(Courier) {A 600 a 600 Aacute 600 aacute 600 Abreve 600 abreve 600 Acircumflex 600 acircumflex 600 acute 600 Adieresis 600 adieresis 600 AE 600 ae 600 Agrave 600 agrave 600 Amacron 600 amacron 600 ampersand 600 Aogonek 600 aogonek 600 Aring 600 aring 600 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 600 asciitilde 600 asterisk 600 at 600 Atilde 600 atilde 600 B 600 b 600 backslash 600 bar 600 braceleft 600 braceright 600 bracketleft 600 bracketright 600 breve 600 brokenbar 600 bullet 600 C 600 c 600 Cacute 600 cacute 600 caron 600 Ccaron 600 ccaron 600 Ccedilla 600 ccedilla 600 cedilla 600 cent 600 center 600 circumflex 600 colon 600 comma 600 commaaccent 600 copyright 600 currency 600 D 600 d 600 dagger 600 daggerdbl 600 Dcaron 600 dcaron 600 Dcroat 600 dcroat 600 dectab 600 degree 600 Delta 600 dieresis 600 divide 600 dollar 600 dotaccent 600 dotlessi 600 down 600 E 600 e 600 Eacute 600 eacute 600 Ecaron 600 ecaron 600 Ecircumflex 600 ecircumflex 600 Edieresis 600 edieresis 600 Edotaccent 600 edotaccent 600 Egrave 600 egrave 600 eight 600 ellipsis 600 Emacron 600 emacron 600 emdash 600 endash 600 Eogonek 600 eogonek 600 equal 600 Eth 600 eth 600 Euro 600 exclam 600 exclamdown 600 F 600 f 600 fi 600 five 600 fl 600 florin 600 format 600 four 600 fraction 600 G 600 g 600 Gbreve 600 gbreve 600 Gcaron 600 gcaron 600 Gcommaaccent 600 gcommaaccent 600 germandbls 600 grave 600 graybox 600 greater 600 greaterequal 600 guillemotleft 600 guillemotright 600 guilsinglleft 600 guilsinglright 600 H 600 h 600 hungarumlaut 600 hyphen 600 I 600 i 600 Iacute 600 iacute 600 Icircumflex 600 icircumflex 600 Idieresis 600 idieresis 600 Idot 600 Idotaccent 600 Igrave 600 igrave 600 IJ 600 ij 600 Imacron 600 imacron 600 indent 600 Iogonek 600 iogonek 600 J 600 j 600 K 600 k 600 Kcommaaccent 600 kcommaaccent 600 L 600 l 600 Lacute 600 lacute 600 largebullet 600 Lcaron 600 lcaron 600 Lcommaaccent 600 lcommaaccent 600 left 600 less 600 lessequal 600 lira 600 LL 600 ll 600 logicalnot 600 lozenge 600 Lslash 600 lslash 600 M 600 m 600 macron 600 merge 600 minus 600 mu 600 multiply 600 N 600 n 600 Nacute 600 nacute 600 Ncaron 600 ncaron 600 Ncommaaccent 600 ncommaaccent 600 nine 600 notegraphic 600 notequal 600 Ntilde 600 ntilde 600 numbersign 600 O 600 o 600 Oacute 600 oacute 600 Ocircumflex 600 ocircumflex 600 Odieresis 600 odieresis 600 OE 600 oe 600 ogonek 600 Ograve 600 ograve 600 Ohungarumlaut 600 ohungarumlaut 600 Omacron 600 omacron 600 one 600 onehalf 600 onequarter 600 onesuperior 600 ordfeminine 600 ordmasculine 600 Oslash 600 oslash 600 Otilde 600 otilde 600 overscore 600 P 600 p 600 paragraph 600 parenleft 600 parenright 600 partialdiff 600 percent 600 period 600 periodcentered 600 perthousand 600 plus 600 plusminus 600 prescription 600 Q 600 q 600 question 600 questiondown 600 quotedbl 600 quotedblbase 600 quotedblleft 600 quotedblright 600 quoteleft 600 quoteright 600 quotesinglbase 600 quotesingle 600 R 600 r 600 Racute 600 racute 600 radical 600 Rcaron 600 rcaron 600 Rcommaaccent 600 rcommaaccent 600 registered 600 return 600 ring 600 S 600 s 600 Sacute 600 sacute 600 Scaron 600 scaron 600 Scedilla 600 scedilla 600 Scommaaccent 600 scommaaccent 600 section 600 semicolon 600 seven 600 six 600 slash 600 space 600 square 600 sterling 600 stop 600 summation 600 T 600 t 600 tab 600 Tcaron 600 tcaron 600 Tcommaaccent 600 tcommaaccent 600 Thorn 600 thorn 600 three 600 threequarters 600 threesuperior 600 tilde 600 trademark 600 two 600 twosuperior 600 U 600 u 600 Uacute 600 uacute 600 Ucircumflex 600 ucircumflex 600 Udieresis 600 udieresis 600 Ugrave 600 ugrave 600 Uhungarumlaut 600 uhungarumlaut 600 Umacron 600 umacron 600 underscore 600 Uogonek 600 uogonek 600 up 600 Uring 600 uring 600 V 600 v 600 W 600 w 600 X 600 x 600 Y 600 y 600 Yacute 600 yacute 600 Ydieresis 600 ydieresis 600 yen 600 Z 600 z 600 Zacute 600 zacute 600 Zcaron 600 zcaron 600 Zdotaccent 600 zdotaccent 600 zero 600}
set font_widths(Courier-Bold) {A 600 a 600 Aacute 600 aacute 600 Abreve 600 abreve 600 Acircumflex 600 acircumflex 600 acute 600 Adieresis 600 adieresis 600 AE 600 ae 600 Agrave 600 agrave 600 Amacron 600 amacron 600 ampersand 600 Aogonek 600 aogonek 600 Aring 600 aring 600 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 600 asciitilde 600 asterisk 600 at 600 Atilde 600 atilde 600 B 600 b 600 backslash 600 bar 600 braceleft 600 braceright 600 bracketleft 600 bracketright 600 breve 600 brokenbar 600 bullet 600 C 600 c 600 Cacute 600 cacute 600 caron 600 Ccaron 600 ccaron 600 Ccedilla 600 ccedilla 600 cedilla 600 cent 600 center 600 circumflex 600 colon 600 comma 600 commaaccent 600 copyright 600 currency 600 D 600 d 600 dagger 600 daggerdbl 600 Dcaron 600 dcaron 600 Dcroat 600 dcroat 600 dectab 600 degree 600 Delta 600 dieresis 600 divide 600 dollar 600 dotaccent 600 dotlessi 600 down 600 E 600 e 600 Eacute 600 eacute 600 Ecaron 600 ecaron 600 Ecircumflex 600 ecircumflex 600 Edieresis 600 edieresis 600 Edotaccent 600 edotaccent 600 Egrave 600 egrave 600 eight 600 ellipsis 600 Emacron 600 emacron 600 emdash 600 endash 600 Eogonek 600 eogonek 600 equal 600 Eth 600 eth 600 Euro 600 exclam 600 exclamdown 600 F 600 f 600 fi 600 five 600 fl 600 florin 600 format 600 four 600 fraction 600 G 600 g 600 Gbreve 600 gbreve 600 Gcaron 600 gcaron 600 Gcommaaccent 600 gcommaaccent 600 germandbls 600 grave 600 graybox 600 greater 600 greaterequal 600 guillemotleft 600 guillemotright 600 guilsinglleft 600 guilsinglright 600 H 600 h 600 hungarumlaut 600 hyphen 600 I 600 i 600 Iacute 600 iacute 600 Icircumflex 600 icircumflex 600 Idieresis 600 idieresis 600 Idot 600 Idotaccent 600 Igrave 600 igrave 600 IJ 600 ij 600 Imacron 600 imacron 600 indent 600 Iogonek 600 iogonek 600 J 600 j 600 K 600 k 600 Kcommaaccent 600 kcommaaccent 600 L 600 l 600 Lacute 600 lacute 600 largebullet 600 Lcaron 600 lcaron 600 Lcommaaccent 600 lcommaaccent 600 left 600 less 600 lessequal 600 lira 600 LL 600 ll 600 logicalnot 600 lozenge 600 Lslash 600 lslash 600 M 600 m 600 macron 600 merge 600 minus 600 mu 600 multiply 600 N 600 n 600 Nacute 600 nacute 600 Ncaron 600 ncaron 600 Ncommaaccent 600 ncommaaccent 600 nine 600 notegraphic 600 notequal 600 Ntilde 600 ntilde 600 numbersign 600 O 600 o 600 Oacute 600 oacute 600 Ocircumflex 600 ocircumflex 600 Odieresis 600 odieresis 600 OE 600 oe 600 ogonek 600 Ograve 600 ograve 600 Ohungarumlaut 600 ohungarumlaut 600 Omacron 600 omacron 600 one 600 onehalf 600 onequarter 600 onesuperior 600 ordfeminine 600 ordmasculine 600 Oslash 600 oslash 600 Otilde 600 otilde 600 overscore 600 P 600 p 600 paragraph 600 parenleft 600 parenright 600 partialdiff 600 percent 600 period 600 periodcentered 600 perthousand 600 plus 600 plusminus 600 prescription 600 Q 600 q 600 question 600 questiondown 600 quotedbl 600 quotedblbase 600 quotedblleft 600 quotedblright 600 quoteleft 600 quoteright 600 quotesinglbase 600 quotesingle 600 R 600 r 600 Racute 600 racute 600 radical 600 Rcaron 600 rcaron 600 Rcommaaccent 600 rcommaaccent 600 registered 600 return 600 ring 600 S 600 s 600 Sacute 600 sacute 600 Scaron 600 scaron 600 Scedilla 600 scedilla 600 Scommaaccent 600 scommaaccent 600 section 600 semicolon 600 seven 600 six 600 slash 600 space 600 square 600 sterling 600 stop 600 summation 600 T 600 t 600 tab 600 Tcaron 600 tcaron 600 Tcommaaccent 600 tcommaaccent 600 Thorn 600 thorn 600 three 600 threequarters 600 threesuperior 600 tilde 600 trademark 600 two 600 twosuperior 600 U 600 u 600 Uacute 600 uacute 600 Ucircumflex 600 ucircumflex 600 Udieresis 600 udieresis 600 Ugrave 600 ugrave 600 Uhungarumlaut 600 uhungarumlaut 600 Umacron 600 umacron 600 underscore 600 Uogonek 600 uogonek 600 up 600 Uring 600 uring 600 V 600 v 600 W 600 w 600 X 600 x 600 Y 600 y 600 Yacute 600 yacute 600 Ydieresis 600 ydieresis 600 yen 600 Z 600 z 600 Zacute 600 zacute 600 Zcaron 600 zcaron 600 Zdotaccent 600 zdotaccent 600 zero 600}
set font_widths(Courier-BoldOblique) {A 600 a 600 Aacute 600 aacute 600 Abreve 600 abreve 600 Acircumflex 600 acircumflex 600 acute 600 Adieresis 600 adieresis 600 AE 600 ae 600 Agrave 600 agrave 600 Amacron 600 amacron 600 ampersand 600 Aogonek 600 aogonek 600 Aring 600 aring 600 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 600 asciitilde 600 asterisk 600 at 600 Atilde 600 atilde 600 B 600 b 600 backslash 600 bar 600 braceleft 600 braceright 600 bracketleft 600 bracketright 600 breve 600 brokenbar 600 bullet 600 C 600 c 600 Cacute 600 cacute 600 caron 600 Ccaron 600 ccaron 600 Ccedilla 600 ccedilla 600 cedilla 600 cent 600 center 600 circumflex 600 colon 600 comma 600 commaaccent 600 copyright 600 currency 600 D 600 d 600 dagger 600 daggerdbl 600 Dcaron 600 dcaron 600 Dcroat 600 dcroat 600 dectab 600 degree 600 Delta 600 dieresis 600 divide 600 dollar 600 dotaccent 600 dotlessi 600 down 600 E 600 e 600 Eacute 600 eacute 600 Ecaron 600 ecaron 600 Ecircumflex 600 ecircumflex 600 Edieresis 600 edieresis 600 Edotaccent 600 edotaccent 600 Egrave 600 egrave 600 eight 600 ellipsis 600 Emacron 600 emacron 600 emdash 600 endash 600 Eogonek 600 eogonek 600 equal 600 Eth 600 eth 600 Euro 600 exclam 600 exclamdown 600 F 600 f 600 fi 600 five 600 fl 600 florin 600 format 600 four 600 fraction 600 G 600 g 600 Gbreve 600 gbreve 600 Gcaron 600 gcaron 600 Gcommaaccent 600 gcommaaccent 600 germandbls 600 grave 600 graybox 600 greater 600 greaterequal 600 guillemotleft 600 guillemotright 600 guilsinglleft 600 guilsinglright 600 H 600 h 600 hungarumlaut 600 hyphen 600 I 600 i 600 Iacute 600 iacute 600 Icircumflex 600 icircumflex 600 Idieresis 600 idieresis 600 Idot 600 Idotaccent 600 Igrave 600 igrave 600 IJ 600 ij 600 Imacron 600 imacron 600 indent 600 Iogonek 600 iogonek 600 J 600 j 600 K 600 k 600 Kcommaaccent 600 kcommaaccent 600 L 600 l 600 Lacute 600 lacute 600 largebullet 600 Lcaron 600 lcaron 600 Lcommaaccent 600 lcommaaccent 600 left 600 less 600 lessequal 600 lira 600 LL 600 ll 600 logicalnot 600 lozenge 600 Lslash 600 lslash 600 M 600 m 600 macron 600 merge 600 minus 600 mu 600 multiply 600 N 600 n 600 Nacute 600 nacute 600 Ncaron 600 ncaron 600 Ncommaaccent 600 ncommaaccent 600 nine 600 notegraphic 600 notequal 600 Ntilde 600 ntilde 600 numbersign 600 O 600 o 600 Oacute 600 oacute 600 Ocircumflex 600 ocircumflex 600 Odieresis 600 odieresis 600 OE 600 oe 600 ogonek 600 Ograve 600 ograve 600 Ohungarumlaut 600 ohungarumlaut 600 Omacron 600 omacron 600 one 600 onehalf 600 onequarter 600 onesuperior 600 ordfeminine 600 ordmasculine 600 Oslash 600 oslash 600 Otilde 600 otilde 600 overscore 600 P 600 p 600 paragraph 600 parenleft 600 parenright 600 partialdiff 600 percent 600 period 600 periodcentered 600 perthousand 600 plus 600 plusminus 600 prescription 600 Q 600 q 600 question 600 questiondown 600 quotedbl 600 quotedblbase 600 quotedblleft 600 quotedblright 600 quoteleft 600 quoteright 600 quotesinglbase 600 quotesingle 600 R 600 r 600 Racute 600 racute 600 radical 600 Rcaron 600 rcaron 600 Rcommaaccent 600 rcommaaccent 600 registered 600 return 600 ring 600 S 600 s 600 Sacute 600 sacute 600 Scaron 600 scaron 600 Scedilla 600 scedilla 600 Scommaaccent 600 scommaaccent 600 section 600 semicolon 600 seven 600 six 600 slash 600 space 600 square 600 sterling 600 stop 600 summation 600 T 600 t 600 tab 600 Tcaron 600 tcaron 600 Tcommaaccent 600 tcommaaccent 600 Thorn 600 thorn 600 three 600 threequarters 600 threesuperior 600 tilde 600 trademark 600 two 600 twosuperior 600 U 600 u 600 Uacute 600 uacute 600 Ucircumflex 600 ucircumflex 600 Udieresis 600 udieresis 600 Ugrave 600 ugrave 600 Uhungarumlaut 600 uhungarumlaut 600 Umacron 600 umacron 600 underscore 600 Uogonek 600 uogonek 600 up 600 Uring 600 uring 600 V 600 v 600 W 600 w 600 X 600 x 600 Y 600 y 600 Yacute 600 yacute 600 Ydieresis 600 ydieresis 600 yen 600 Z 600 z 600 Zacute 600 zacute 600 Zcaron 600 zcaron 600 Zdotaccent 600 zdotaccent 600 zero 600}
set font_widths(Courier-Oblique) {A 600 a 600 Aacute 600 aacute 600 Abreve 600 abreve 600 Acircumflex 600 acircumflex 600 acute 600 Adieresis 600 adieresis 600 AE 600 ae 600 Agrave 600 agrave 600 Amacron 600 amacron 600 ampersand 600 Aogonek 600 aogonek 600 Aring 600 aring 600 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 600 asciitilde 600 asterisk 600 at 600 Atilde 600 atilde 600 B 600 b 600 backslash 600 bar 600 braceleft 600 braceright 600 bracketleft 600 bracketright 600 breve 600 brokenbar 600 bullet 600 C 600 c 600 Cacute 600 cacute 600 caron 600 Ccaron 600 ccaron 600 Ccedilla 600 ccedilla 600 cedilla 600 cent 600 center 600 circumflex 600 colon 600 comma 600 commaaccent 600 copyright 600 currency 600 D 600 d 600 dagger 600 daggerdbl 600 Dcaron 600 dcaron 600 Dcroat 600 dcroat 600 dectab 600 degree 600 Delta 600 dieresis 600 divide 600 dollar 600 dotaccent 600 dotlessi 600 down 600 E 600 e 600 Eacute 600 eacute 600 Ecaron 600 ecaron 600 Ecircumflex 600 ecircumflex 600 Edieresis 600 edieresis 600 Edotaccent 600 edotaccent 600 Egrave 600 egrave 600 eight 600 ellipsis 600 Emacron 600 emacron 600 emdash 600 endash 600 Eogonek 600 eogonek 600 equal 600 Eth 600 eth 600 Euro 600 exclam 600 exclamdown 600 F 600 f 600 fi 600 five 600 fl 600 florin 600 format 600 four 600 fraction 600 G 600 g 600 Gbreve 600 gbreve 600 Gcaron 600 gcaron 600 Gcommaaccent 600 gcommaaccent 600 germandbls 600 grave 600 graybox 600 greater 600 greaterequal 600 guillemotleft 600 guillemotright 600 guilsinglleft 600 guilsinglright 600 H 600 h 600 hungarumlaut 600 hyphen 600 I 600 i 600 Iacute 600 iacute 600 Icircumflex 600 icircumflex 600 Idieresis 600 idieresis 600 Idot 600 Idotaccent 600 Igrave 600 igrave 600 IJ 600 ij 600 Imacron 600 imacron 600 indent 600 Iogonek 600 iogonek 600 J 600 j 600 K 600 k 600 Kcommaaccent 600 kcommaaccent 600 L 600 l 600 Lacute 600 lacute 600 largebullet 600 Lcaron 600 lcaron 600 Lcommaaccent 600 lcommaaccent 600 left 600 less 600 lessequal 600 lira 600 LL 600 ll 600 logicalnot 600 lozenge 600 Lslash 600 lslash 600 M 600 m 600 macron 600 merge 600 minus 600 mu 600 multiply 600 N 600 n 600 Nacute 600 nacute 600 Ncaron 600 ncaron 600 Ncommaaccent 600 ncommaaccent 600 nine 600 notegraphic 600 notequal 600 Ntilde 600 ntilde 600 numbersign 600 O 600 o 600 Oacute 600 oacute 600 Ocircumflex 600 ocircumflex 600 Odieresis 600 odieresis 600 OE 600 oe 600 ogonek 600 Ograve 600 ograve 600 Ohungarumlaut 600 ohungarumlaut 600 Omacron 600 omacron 600 one 600 onehalf 600 onequarter 600 onesuperior 600 ordfeminine 600 ordmasculine 600 Oslash 600 oslash 600 Otilde 600 otilde 600 overscore 600 P 600 p 600 paragraph 600 parenleft 600 parenright 600 partialdiff 600 percent 600 period 600 periodcentered 600 perthousand 600 plus 600 plusminus 600 prescription 600 Q 600 q 600 question 600 questiondown 600 quotedbl 600 quotedblbase 600 quotedblleft 600 quotedblright 600 quoteleft 600 quoteright 600 quotesinglbase 600 quotesingle 600 R 600 r 600 Racute 600 racute 600 radical 600 Rcaron 600 rcaron 600 Rcommaaccent 600 rcommaaccent 600 registered 600 return 600 ring 600 S 600 s 600 Sacute 600 sacute 600 Scaron 600 scaron 600 Scedilla 600 scedilla 600 Scommaaccent 600 scommaaccent 600 section 600 semicolon 600 seven 600 six 600 slash 600 space 600 square 600 sterling 600 stop 600 summation 600 T 600 t 600 tab 600 Tcaron 600 tcaron 600 Tcommaaccent 600 tcommaaccent 600 Thorn 600 thorn 600 three 600 threequarters 600 threesuperior 600 tilde 600 trademark 600 two 600 twosuperior 600 U 600 u 600 Uacute 600 uacute 600 Ucircumflex 600 ucircumflex 600 Udieresis 600 udieresis 600 Ugrave 600 ugrave 600 Uhungarumlaut 600 uhungarumlaut 600 Umacron 600 umacron 600 underscore 600 Uogonek 600 uogonek 600 up 600 Uring 600 uring 600 V 600 v 600 W 600 w 600 X 600 x 600 Y 600 y 600 Yacute 600 yacute 600 Ydieresis 600 ydieresis 600 yen 600 Z 600 z 600 Zacute 600 zacute 600 Zcaron 600 zcaron 600 Zdotaccent 600 zdotaccent 600 zero 600}
set font_widths(Helvetica) {A 667 a 556 Aacute 667 aacute 556 Abreve 667 abreve 556 Acircumflex 667 acircumflex 556 acute 333 Adieresis 667 adieresis 556 AE 1000 ae 889 Agrave 667 agrave 556 Amacron 667 amacron 556 ampersand 667 Aogonek 667 aogonek 556 Aring 667 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 469 asciitilde 584 asterisk 389 at 1015 Atilde 667 atilde 556 B 667 b 556 backslash 278 bar 260 braceleft 334 braceright 334 bracketleft 278 bracketright 278 breve 333 brokenbar 260 bullet 350 C 722 c 500 Cacute 722 cacute 500 caron 333 Ccaron 722 ccaron 500 Ccedilla 722 ccedilla 500 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 commaaccent 250 copyright 737 currency 556 D 722 d 556 dagger 556 daggerdbl 556 Dcaron 722 dcaron 643 Dcroat 722 dcroat 556 dectab 600 degree 400 Delta 612 dieresis 333 divide 584 dollar 556 dotaccent 333 dotlessi 278 down 600 E 667 e 556 Eacute 667 eacute 556 Ecaron 667 ecaron 556 Ecircumflex 667 ecircumflex 556 Edieresis 667 edieresis 556 Edotaccent 667 edotaccent 556 Egrave 667 egrave 556 eight 556 ellipsis 1000 Emacron 667 emacron 556 emdash 1000 endash 556 Eogonek 667 eogonek 556 equal 584 Eth 722 eth 556 Euro 556 exclam 278 exclamdown 333 F 611 f 278 fi 500 five 556 fl 500 florin 556 format 600 four 556 fraction 167 G 778 g 556 Gbreve 778 gbreve 556 Gcaron 600 gcaron 600 Gcommaaccent 778 gcommaaccent 556 germandbls 611 grave 333 graybox 600 greater 584 greaterequal 549 guillemotleft 556 guillemotright 556 guilsinglleft 333 guilsinglright 333 H 722 h 556 hungarumlaut 333 hyphen 333 I 278 i 222 Iacute 278 iacute 278 Icircumflex 278 icircumflex 278 Idieresis 278 idieresis 278 Idot 600 Idotaccent 278 Igrave 278 igrave 278 IJ 600 ij 600 Imacron 278 imacron 278 indent 600 Iogonek 278 iogonek 222 J 500 j 222 K 667 k 500 Kcommaaccent 667 kcommaaccent 500 L 556 l 222 Lacute 556 lacute 222 largebullet 600 Lcaron 556 lcaron 299 Lcommaaccent 556 lcommaaccent 222 left 600 less 584 lessequal 549 lira 600 LL 600 ll 600 logicalnot 584 lozenge 471 Lslash 556 lslash 222 M 833 m 833 macron 333 merge 600 minus 584 mu 556 multiply 584 N 722 n 556 Nacute 722 nacute 556 Ncaron 722 ncaron 556 Ncommaaccent 722 ncommaaccent 556 nine 556 notegraphic 600 notequal 549 Ntilde 722 ntilde 556 numbersign 556 O 778 o 556 Oacute 778 oacute 556 Ocircumflex 778 ocircumflex 556 Odieresis 778 odieresis 556 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 556 Ohungarumlaut 778 ohungarumlaut 556 Omacron 778 omacron 556 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 370 ordmasculine 365 Oslash 778 oslash 611 Otilde 778 otilde 556 overscore 600 P 667 p 556 paragraph 537 parenleft 333 parenright 333 partialdiff 476 percent 889 period 278 periodcentered 278 perthousand 1000 plus 584 plusminus 584 prescription 600 Q 778 q 556 question 556 questiondown 611 quotedbl 355 quotedblbase 333 quotedblleft 333 quotedblright 333 quoteleft 222 quoteright 222 quotesinglbase 222 quotesingle 191 R 722 r 333 Racute 722 racute 333 radical 453 Rcaron 722 rcaron 333 Rcommaaccent 722 rcommaaccent 333 registered 737 return 600 ring 333 S 667 s 500 Sacute 667 sacute 500 Scaron 667 scaron 500 Scedilla 667 scedilla 500 Scommaaccent 667 scommaaccent 500 section 556 semicolon 278 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 summation 600 T 611 t 278 tab 600 Tcaron 611 tcaron 317 Tcommaaccent 611 tcommaaccent 278 Thorn 667 thorn 556 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 1000 two 556 twosuperior 333 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 Uhungarumlaut 722 uhungarumlaut 556 Umacron 722 umacron 556 underscore 556 Uogonek 722 uogonek 556 up 600 Uring 722 uring 556 V 667 v 500 W 944 w 722 X 667 x 500 Y 667 y 500 Yacute 667 yacute 500 Ydieresis 667 ydieresis 500 yen 556 Z 611 z 500 Zacute 611 zacute 500 Zcaron 611 zcaron 500 Zdotaccent 611 zdotaccent 500 zero 556}
set font_widths(Helvetica-Bold) {A 722 a 556 Aacute 722 aacute 556 Abreve 722 abreve 556 Acircumflex 722 acircumflex 556 acute 333 Adieresis 722 adieresis 556 AE 1000 ae 889 Agrave 722 agrave 556 Amacron 722 amacron 556 ampersand 722 Aogonek 722 aogonek 556 Aring 722 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 584 asciitilde 584 asterisk 389 at 975 Atilde 722 atilde 556 B 722 b 611 backslash 278 bar 280 braceleft 389 braceright 389 bracketleft 333 bracketright 333 breve 333 brokenbar 280 bullet 350 C 722 c 556 Cacute 722 cacute 556 caron 333 Ccaron 722 ccaron 556 Ccedilla 722 ccedilla 556 cedilla 333 cent 556 center 600 circumflex 333 colon 333 comma 278 commaaccent 250 copyright 737 currency 556 D 722 d 611 dagger 556 daggerdbl 556 Dcaron 722 dcaron 743 Dcroat 722 dcroat 611 dectab 600 degree 400 Delta 612 dieresis 333 divide 584 dollar 556 dotaccent 333 dotlessi 278 down 600 E 667 e 556 Eacute 667 eacute 556 Ecaron 667 ecaron 556 Ecircumflex 667 ecircumflex 556 Edieresis 667 edieresis 556 Edotaccent 667 edotaccent 556 Egrave 667 egrave 556 eight 556 ellipsis 1000 Emacron 667 emacron 556 emdash 1000 endash 556 Eogonek 667 eogonek 556 equal 584 Eth 722 eth 611 Euro 556 exclam 333 exclamdown 333 F 611 f 333 fi 611 five 556 fl 611 florin 556 format 600 four 556 fraction 167 G 778 g 611 Gbreve 778 gbreve 611 Gcaron 600 gcaron 600 Gcommaaccent 778 gcommaaccent 611 germandbls 611 grave 333 graybox 600 greater 584 greaterequal 549 guillemotleft 556 guillemotright 556 guilsinglleft 333 guilsinglright 333 H 722 h 611 hungarumlaut 333 hyphen 333 I 278 i 278 Iacute 278 iacute 278 Icircumflex 278 icircumflex 278 Idieresis 278 idieresis 278 Idot 600 Idotaccent 278 Igrave 278 igrave 278 IJ 600 ij 600 Imacron 278 imacron 278 indent 600 Iogonek 278 iogonek 278 J 556 j 278 K 722 k 556 Kcommaaccent 722 kcommaaccent 556 L 611 l 278 Lacute 611 lacute 278 largebullet 600 Lcaron 611 lcaron 400 Lcommaaccent 611 lcommaaccent 278 left 600 less 584 lessequal 549 lira 600 LL 600 ll 600 logicalnot 584 lozenge 494 Lslash 611 lslash 278 M 833 m 889 macron 333 merge 600 minus 584 mu 611 multiply 584 N 722 n 611 Nacute 722 nacute 611 Ncaron 722 ncaron 611 Ncommaaccent 722 ncommaaccent 611 nine 556 notegraphic 600 notequal 549 Ntilde 722 ntilde 611 numbersign 556 O 778 o 611 Oacute 778 oacute 611 Ocircumflex 778 ocircumflex 611 Odieresis 778 odieresis 611 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 611 Ohungarumlaut 778 ohungarumlaut 611 Omacron 778 omacron 611 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 370 ordmasculine 365 Oslash 778 oslash 611 Otilde 778 otilde 611 overscore 600 P 667 p 611 paragraph 556 parenleft 333 parenright 333 partialdiff 494 percent 889 period 278 periodcentered 278 perthousand 1000 plus 584 plusminus 584 prescription 600 Q 778 q 611 question 611 questiondown 611 quotedbl 474 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 238 R 722 r 389 Racute 722 racute 389 radical 549 Rcaron 722 rcaron 389 Rcommaaccent 722 rcommaaccent 389 registered 737 return 600 ring 333 S 667 s 556 Sacute 667 sacute 556 Scaron 667 scaron 556 Scedilla 667 scedilla 556 Scommaaccent 667 scommaaccent 556 section 556 semicolon 333 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 summation 600 T 611 t 333 tab 600 Tcaron 611 tcaron 389 Tcommaaccent 611 tcommaaccent 333 Thorn 667 thorn 611 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 1000 two 556 twosuperior 333 U 722 u 611 Uacute 722 uacute 611 Ucircumflex 722 ucircumflex 611 Udieresis 722 udieresis 611 Ugrave 722 ugrave 611 Uhungarumlaut 722 uhungarumlaut 611 Umacron 722 umacron 611 underscore 556 Uogonek 722 uogonek 611 up 600 Uring 722 uring 611 V 667 v 556 W 944 w 778 X 667 x 556 Y 667 y 556 Yacute 667 yacute 556 Ydieresis 667 ydieresis 556 yen 556 Z 611 z 500 Zacute 611 zacute 500 Zcaron 611 zcaron 500 Zdotaccent 611 zdotaccent 500 zero 556}
set font_widths(Helvetica-BoldOblique) {A 722 a 556 Aacute 722 aacute 556 Abreve 722 abreve 556 Acircumflex 722 acircumflex 556 acute 333 Adieresis 722 adieresis 556 AE 1000 ae 889 Agrave 722 agrave 556 Amacron 722 amacron 556 ampersand 722 Aogonek 722 aogonek 556 Aring 722 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 584 asciitilde 584 asterisk 389 at 975 Atilde 722 atilde 556 B 722 b 611 backslash 278 bar 280 braceleft 389 braceright 389 bracketleft 333 bracketright 333 breve 333 brokenbar 280 bullet 350 C 722 c 556 Cacute 722 cacute 556 caron 333 Ccaron 722 ccaron 556 Ccedilla 722 ccedilla 556 cedilla 333 cent 556 center 600 circumflex 333 colon 333 comma 278 commaaccent 250 copyright 737 currency 556 D 722 d 611 dagger 556 daggerdbl 556 Dcaron 722 dcaron 743 Dcroat 722 dcroat 611 dectab 600 degree 400 Delta 612 dieresis 333 divide 584 dollar 556 dotaccent 333 dotlessi 278 down 600 E 667 e 556 Eacute 667 eacute 556 Ecaron 667 ecaron 556 Ecircumflex 667 ecircumflex 556 Edieresis 667 edieresis 556 Edotaccent 667 edotaccent 556 Egrave 667 egrave 556 eight 556 ellipsis 1000 Emacron 667 emacron 556 emdash 1000 endash 556 Eogonek 667 eogonek 556 equal 584 Eth 722 eth 611 Euro 556 exclam 333 exclamdown 333 F 611 f 333 fi 611 five 556 fl 611 florin 556 format 600 four 556 fraction 167 G 778 g 611 Gbreve 778 gbreve 611 Gcaron 600 gcaron 600 Gcommaaccent 778 gcommaaccent 611 germandbls 611 grave 333 graybox 600 greater 584 greaterequal 549 guillemotleft 556 guillemotright 556 guilsinglleft 333 guilsinglright 333 H 722 h 611 hungarumlaut 333 hyphen 333 I 278 i 278 Iacute 278 iacute 278 Icircumflex 278 icircumflex 278 Idieresis 278 idieresis 278 Idot 600 Idotaccent 278 Igrave 278 igrave 278 IJ 600 ij 600 Imacron 278 imacron 278 indent 600 Iogonek 278 iogonek 278 J 556 j 278 K 722 k 556 Kcommaaccent 722 kcommaaccent 556 L 611 l 278 Lacute 611 lacute 278 largebullet 600 Lcaron 611 lcaron 400 Lcommaaccent 611 lcommaaccent 278 left 600 less 584 lessequal 549 lira 600 LL 600 ll 600 logicalnot 584 lozenge 494 Lslash 611 lslash 278 M 833 m 889 macron 333 merge 600 minus 584 mu 611 multiply 584 N 722 n 611 Nacute 722 nacute 611 Ncaron 722 ncaron 611 Ncommaaccent 722 ncommaaccent 611 nine 556 notegraphic 600 notequal 549 Ntilde 722 ntilde 611 numbersign 556 O 778 o 611 Oacute 778 oacute 611 Ocircumflex 778 ocircumflex 611 Odieresis 778 odieresis 611 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 611 Ohungarumlaut 778 ohungarumlaut 611 Omacron 778 omacron 611 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 370 ordmasculine 365 Oslash 778 oslash 611 Otilde 778 otilde 611 overscore 600 P 667 p 611 paragraph 556 parenleft 333 parenright 333 partialdiff 494 percent 889 period 278 periodcentered 278 perthousand 1000 plus 584 plusminus 584 prescription 600 Q 778 q 611 question 611 questiondown 611 quotedbl 474 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 238 R 722 r 389 Racute 722 racute 389 radical 549 Rcaron 722 rcaron 389 Rcommaaccent 722 rcommaaccent 389 registered 737 return 600 ring 333 S 667 s 556 Sacute 667 sacute 556 Scaron 667 scaron 556 Scedilla 667 scedilla 556 Scommaaccent 667 scommaaccent 556 section 556 semicolon 333 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 summation 600 T 611 t 333 tab 600 Tcaron 611 tcaron 389 Tcommaaccent 611 tcommaaccent 333 Thorn 667 thorn 611 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 1000 two 556 twosuperior 333 U 722 u 611 Uacute 722 uacute 611 Ucircumflex 722 ucircumflex 611 Udieresis 722 udieresis 611 Ugrave 722 ugrave 611 Uhungarumlaut 722 uhungarumlaut 611 Umacron 722 umacron 611 underscore 556 Uogonek 722 uogonek 611 up 600 Uring 722 uring 611 V 667 v 556 W 944 w 778 X 667 x 556 Y 667 y 556 Yacute 667 yacute 556 Ydieresis 667 ydieresis 556 yen 556 Z 611 z 500 Zacute 611 zacute 500 Zcaron 611 zcaron 500 Zdotaccent 611 zdotaccent 500 zero 556}
set font_widths(Helvetica-Light) {A 667 a 556 Aacute 667 aacute 556 Acircumflex 667 acircumflex 556 acute 333 Adieresis 667 adieresis 556 AE 1000 ae 889 Agrave 667 agrave 556 ampersand 667 Aring 667 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 660 asciitilde 660 asterisk 389 at 800 Atilde 667 atilde 556 B 667 b 611 backslash 278 bar 222 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 222 bullet 500 C 722 c 556 caron 333 Ccedilla 722 ccedilla 556 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 copyright 800 currency 556 D 722 d 611 dagger 556 daggerdbl 556 dectab 600 degree 400 dieresis 333 divide 660 dollar 556 dotaccent 333 dotlessi 222 down 600 E 611 e 556 Eacute 611 eacute 556 Ecircumflex 611 ecircumflex 556 Edieresis 611 edieresis 556 Egrave 611 egrave 556 eight 556 ellipsis 1000 emdash 1000 endash 500 equal 660 Eth 722 eth 556 exclam 333 exclamdown 333 F 556 f 278 fi 500 five 556 fl 500 florin 556 format 600 four 556 fraction 167 G 778 g 611 Gcaron 600 gcaron 600 germandbls 500 grave 333 graybox 600 greater 660 guillemotleft 556 guillemotright 556 guilsinglleft 389 guilsinglright 389 H 722 h 556 hungarumlaut 333 hyphen 333 I 278 i 222 Iacute 278 iacute 222 Icircumflex 278 icircumflex 222 Idieresis 278 idieresis 222 Idot 600 Igrave 278 igrave 222 IJ 600 ij 600 indent 600 J 500 j 222 K 667 k 500 L 556 l 222 largebullet 600 left 600 less 660 lira 600 LL 600 ll 600 logicalnot 660 Lslash 556 lslash 222 M 833 m 833 macron 333 merge 600 minus 660 mu 556 multiply 660 N 722 n 556 nine 556 notegraphic 600 Ntilde 722 ntilde 556 numbersign 556 O 778 o 556 Oacute 778 oacute 556 Ocircumflex 778 ocircumflex 556 Odieresis 778 odieresis 556 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 556 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 334 ordmasculine 334 Oslash 778 oslash 556 Otilde 778 otilde 556 overscore 600 P 611 p 611 paragraph 650 parenleft 333 parenright 333 percent 889 period 278 periodcentered 278 perthousand 1000 plus 660 plusminus 660 prescription 600 Q 778 q 611 question 500 questiondown 500 quotedbl 278 quotedblbase 389 quotedblleft 389 quotedblright 389 quoteleft 222 quoteright 222 quotesinglbase 222 quotesingle 222 R 667 r 333 registered 800 return 600 ring 333 S 611 s 500 Scaron 611 scaron 500 Scedilla 600 scedilla 600 section 556 semicolon 278 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 T 556 t 278 tab 600 Thorn 611 thorn 611 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 940 two 556 twosuperior 333 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 underscore 500 up 600 V 611 v 500 W 889 w 722 X 611 x 500 Y 611 y 500 Yacute 611 yacute 500 Ydieresis 611 ydieresis 500 yen 556 Z 611 z 500 Zcaron 611 zcaron 500 zero 556}
set font_widths(Helvetica-LightOblique) {A 667 a 556 Aacute 667 aacute 556 Acircumflex 667 acircumflex 556 acute 333 Adieresis 667 adieresis 556 AE 1000 ae 889 Agrave 667 agrave 556 ampersand 667 Aring 667 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 660 asciitilde 660 asterisk 389 at 800 Atilde 667 atilde 556 B 667 b 611 backslash 278 bar 222 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 222 bullet 500 C 722 c 556 caron 333 Ccedilla 722 ccedilla 556 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 copyright 800 currency 556 D 722 d 611 dagger 556 daggerdbl 556 dectab 600 degree 400 dieresis 333 divide 660 dollar 556 dotaccent 333 dotlessi 222 down 600 E 611 e 556 Eacute 611 eacute 556 Ecircumflex 611 ecircumflex 556 Edieresis 611 edieresis 556 Egrave 611 egrave 556 eight 556 ellipsis 1000 emdash 1000 endash 500 equal 660 Eth 722 eth 556 exclam 333 exclamdown 333 F 556 f 278 fi 500 five 556 fl 500 florin 556 format 600 four 556 fraction 167 G 778 g 611 Gcaron 600 gcaron 600 germandbls 500 grave 333 graybox 600 greater 660 guillemotleft 556 guillemotright 556 guilsinglleft 389 guilsinglright 389 H 722 h 556 hungarumlaut 333 hyphen 333 I 278 i 222 Iacute 278 iacute 222 Icircumflex 278 icircumflex 222 Idieresis 278 idieresis 222 Idot 600 Igrave 278 igrave 222 IJ 600 ij 600 indent 600 J 500 j 222 K 667 k 500 L 556 l 222 largebullet 600 left 600 less 660 lira 600 LL 600 ll 600 logicalnot 660 Lslash 556 lslash 222 M 833 m 833 macron 333 merge 600 minus 660 mu 556 multiply 660 N 722 n 556 nine 556 notegraphic 600 Ntilde 722 ntilde 556 numbersign 556 O 778 o 556 Oacute 778 oacute 556 Ocircumflex 778 ocircumflex 556 Odieresis 778 odieresis 556 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 556 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 334 ordmasculine 334 Oslash 778 oslash 556 Otilde 778 otilde 556 overscore 600 P 611 p 611 paragraph 650 parenleft 333 parenright 333 percent 889 period 278 periodcentered 278 perthousand 1000 plus 660 plusminus 660 prescription 600 Q 778 q 611 question 500 questiondown 500 quotedbl 278 quotedblbase 389 quotedblleft 389 quotedblright 389 quoteleft 222 quoteright 222 quotesinglbase 222 quotesingle 222 R 667 r 333 registered 800 return 600 ring 333 S 611 s 500 Scaron 611 scaron 500 Scedilla 600 scedilla 600 section 556 semicolon 278 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 T 556 t 278 tab 600 Thorn 611 thorn 611 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 940 two 556 twosuperior 333 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 underscore 500 up 600 V 611 v 500 W 889 w 722 X 611 x 500 Y 611 y 500 Yacute 611 yacute 500 Ydieresis 611 ydieresis 500 yen 556 Z 611 z 500 Zcaron 611 zcaron 500 zero 556}
set font_widths(Helvetica-Narrow) {A 547 a 456 Aacute 547 aacute 456 Acircumflex 547 acircumflex 456 acute 273 Adieresis 547 adieresis 456 AE 820 ae 729 Agrave 547 agrave 456 ampersand 547 Aring 547 aring 456 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 385 asciitilde 479 asterisk 319 at 832 Atilde 547 atilde 456 B 547 b 456 backslash 228 bar 213 braceleft 274 braceright 274 bracketleft 228 bracketright 228 breve 273 brokenbar 213 bullet 287 C 592 c 410 caron 273 Ccedilla 592 ccedilla 410 cedilla 273 cent 456 center 600 circumflex 273 colon 228 comma 228 copyright 604 currency 456 D 592 d 456 dagger 456 daggerdbl 456 dectab 600 degree 328 dieresis 273 divide 479 dollar 456 dotaccent 273 dotlessi 228 down 600 E 547 e 456 Eacute 547 eacute 456 Ecircumflex 547 ecircumflex 456 Edieresis 547 edieresis 456 Egrave 547 egrave 456 eight 456 ellipsis 820 emdash 820 endash 456 equal 479 Eth 592 eth 456 exclam 228 exclamdown 273 F 501 f 228 fi 410 five 456 fl 410 florin 456 format 600 four 456 fraction 137 G 638 g 456 Gcaron 600 gcaron 600 germandbls 501 grave 273 graybox 600 greater 479 guillemotleft 456 guillemotright 456 guilsinglleft 273 guilsinglright 273 H 592 h 456 hungarumlaut 273 hyphen 273 I 228 i 182 Iacute 228 iacute 228 Icircumflex 228 icircumflex 228 Idieresis 228 idieresis 228 Idot 600 Igrave 228 igrave 228 IJ 600 ij 600 indent 600 J 410 j 182 K 547 k 410 L 456 l 182 largebullet 600 left 600 less 479 lira 600 LL 600 ll 600 logicalnot 479 Lslash 456 lslash 182 M 683 m 683 macron 273 merge 600 minus 479 mu 456 multiply 479 N 592 n 456 nine 456 notegraphic 600 Ntilde 592 ntilde 456 numbersign 456 O 638 o 456 Oacute 638 oacute 456 Ocircumflex 638 ocircumflex 456 Odieresis 638 odieresis 456 OE 820 oe 774 ogonek 273 Ograve 638 ograve 456 one 456 onehalf 684 onequarter 684 onesuperior 273 ordfeminine 303 ordmasculine 299 Oslash 638 oslash 501 Otilde 638 otilde 456 overscore 600 P 547 p 456 paragraph 440 parenleft 273 parenright 273 percent 729 period 228 periodcentered 228 perthousand 820 plus 479 plusminus 479 prescription 600 Q 638 q 456 question 456 questiondown 501 quotedbl 291 quotedblbase 273 quotedblleft 273 quotedblright 273 quoteleft 182 quoteright 182 quotesinglbase 182 quotesingle 157 R 592 r 273 registered 604 return 600 ring 273 S 547 s 410 Scaron 547 scaron 410 Scedilla 600 scedilla 600 section 456 semicolon 228 seven 456 six 456 slash 228 space 228 square 600 sterling 456 stop 600 T 501 t 228 tab 600 Thorn 547 thorn 456 three 456 threequarters 684 threesuperior 273 tilde 273 trademark 820 two 456 twosuperior 273 U 592 u 456 Uacute 592 uacute 456 Ucircumflex 592 ucircumflex 456 Udieresis 592 udieresis 456 Ugrave 592 ugrave 456 underscore 456 up 600 V 547 v 410 W 774 w 592 X 547 x 410 Y 547 y 410 Yacute 547 yacute 410 Ydieresis 547 ydieresis 410 yen 456 Z 501 z 410 Zcaron 501 zcaron 410 zero 456}
set font_widths(Helvetica-Narrow-Bold) {A 592 a 456 Aacute 592 aacute 456 Acircumflex 592 acircumflex 456 acute 273 Adieresis 592 adieresis 456 AE 820 ae 729 Agrave 592 agrave 456 ampersand 592 Aring 592 aring 456 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 479 asciitilde 479 asterisk 319 at 800 Atilde 592 atilde 456 B 592 b 501 backslash 228 bar 230 braceleft 319 braceright 319 bracketleft 273 bracketright 273 breve 273 brokenbar 230 bullet 287 C 592 c 456 caron 273 Ccedilla 592 ccedilla 456 cedilla 273 cent 456 center 600 circumflex 273 colon 273 comma 228 copyright 604 currency 456 D 592 d 501 dagger 456 daggerdbl 456 dectab 600 degree 328 dieresis 273 divide 479 dollar 456 dotaccent 273 dotlessi 228 down 600 E 547 e 456 Eacute 547 eacute 456 Ecircumflex 547 ecircumflex 456 Edieresis 547 edieresis 456 Egrave 547 egrave 456 eight 456 ellipsis 820 emdash 820 endash 456 equal 479 Eth 592 eth 501 exclam 273 exclamdown 273 F 501 f 273 fi 501 five 456 fl 501 florin 456 format 600 four 456 fraction 137 G 638 g 501 Gcaron 600 gcaron 600 germandbls 501 grave 273 graybox 600 greater 479 guillemotleft 456 guillemotright 456 guilsinglleft 273 guilsinglright 273 H 592 h 501 hungarumlaut 273 hyphen 273 I 228 i 228 Iacute 228 iacute 228 Icircumflex 228 icircumflex 228 Idieresis 228 idieresis 228 Idot 600 Igrave 228 igrave 228 IJ 600 ij 600 indent 600 J 456 j 228 K 592 k 456 L 501 l 228 largebullet 600 left 600 less 479 lira 600 LL 600 ll 600 logicalnot 479 Lslash 501 lslash 228 M 683 m 729 macron 273 merge 600 minus 479 mu 501 multiply 479 N 592 n 501 nine 456 notegraphic 600 Ntilde 592 ntilde 501 numbersign 456 O 638 o 501 Oacute 638 oacute 501 Ocircumflex 638 ocircumflex 501 Odieresis 638 odieresis 501 OE 820 oe 774 ogonek 273 Ograve 638 ograve 501 one 456 onehalf 684 onequarter 684 onesuperior 273 ordfeminine 303 ordmasculine 299 Oslash 638 oslash 501 Otilde 638 otilde 501 overscore 600 P 547 p 501 paragraph 456 parenleft 273 parenright 273 percent 729 period 228 periodcentered 228 perthousand 820 plus 479 plusminus 479 prescription 600 Q 638 q 501 question 501 questiondown 501 quotedbl 389 quotedblbase 410 quotedblleft 410 quotedblright 410 quoteleft 228 quoteright 228 quotesinglbase 228 quotesingle 195 R 592 r 319 registered 604 return 600 ring 273 S 547 s 456 Scaron 547 scaron 456 Scedilla 600 scedilla 600 section 456 semicolon 273 seven 456 six 456 slash 228 space 228 square 600 sterling 456 stop 600 T 501 t 273 tab 600 Thorn 547 thorn 501 three 456 threequarters 684 threesuperior 273 tilde 273 trademark 820 two 456 twosuperior 273 U 592 u 501 Uacute 592 uacute 501 Ucircumflex 592 ucircumflex 501 Udieresis 592 udieresis 501 Ugrave 592 ugrave 501 underscore 456 up 600 V 547 v 456 W 774 w 638 X 547 x 456 Y 547 y 456 Yacute 547 yacute 456 Ydieresis 547 ydieresis 456 yen 456 Z 501 z 410 Zcaron 501 zcaron 410 zero 456}
set font_widths(Helvetica-Narrow-BoldOblique) {A 592 a 456 Aacute 592 aacute 456 Acircumflex 592 acircumflex 456 acute 273 Adieresis 592 adieresis 456 AE 820 ae 729 Agrave 592 agrave 456 ampersand 592 Aring 592 aring 456 asciicircum 479 asciitilde 479 asterisk 319 at 800 Atilde 592 atilde 456 B 592 b 501 backslash 228 bar 230 braceleft 319 braceright 319 bracketleft 273 bracketright 273 breve 273 brokenbar 230 bullet 287 C 592 c 456 caron 273 Ccedilla 592 ccedilla 456 cedilla 273 cent 456 circumflex 273 colon 273 comma 228 copyright 604 currency 456 D 592 d 501 dagger 456 daggerdbl 456 degree 328 dieresis 273 divide 479 dollar 456 dotaccent 273 dotlessi 228 E 547 e 456 Eacute 547 eacute 456 Ecircumflex 547 ecircumflex 456 Edieresis 547 edieresis 456 Egrave 547 egrave 456 eight 456 ellipsis 820 emdash 820 endash 456 equal 479 Eth 592 eth 501 exclam 273 exclamdown 273 F 501 f 273 fi 501 five 456 fl 501 florin 456 four 456 fraction 137 G 638 g 501 germandbls 501 grave 273 greater 479 guillemotleft 456 guillemotright 456 guilsinglleft 273 guilsinglright 273 H 592 h 501 hungarumlaut 273 hyphen 273 I 228 i 228 Iacute 228 iacute 228 Icircumflex 228 icircumflex 228 Idieresis 228 idieresis 228 Igrave 228 igrave 228 J 456 j 228 K 592 k 456 L 501 l 228 less 479 logicalnot 479 Lslash 501 lslash 228 M 683 m 729 macron 273 minus 479 mu 501 multiply 479 N 592 n 501 nine 456 Ntilde 592 ntilde 501 numbersign 456 O 638 o 501 Oacute 638 oacute 501 Ocircumflex 638 ocircumflex 501 Odieresis 638 odieresis 501 OE 820 oe 774 ogonek 273 Ograve 638 ograve 501 one 456 onehalf 684 onequarter 684 onesuperior 273 ordfeminine 303 ordmasculine 299 Oslash 638 oslash 501 Otilde 638 otilde 501 P 547 p 501 paragraph 456 parenleft 273 parenright 273 percent 729 period 228 periodcentered 228 perthousand 820 plus 479 plusminus 479 Q 638 q 501 question 501 questiondown 501 quotedbl 389 quotedblbase 410 quotedblleft 410 quotedblright 410 quoteleft 228 quoteright 228 quotesinglbase 228 quotesingle 195 R 592 r 319 registered 604 ring 273 S 547 s 456 Scaron 547 scaron 456 section 456 semicolon 273 seven 456 six 456 slash 228 space 228 sterling 456 T 501 t 273 Thorn 547 thorn 501 three 456 threequarters 684 threesuperior 273 tilde 273 trademark 820 two 456 twosuperior 273 U 592 u 501 Uacute 592 uacute 501 Ucircumflex 592 ucircumflex 501 Udieresis 592 udieresis 501 Ugrave 592 ugrave 501 underscore 456 V 547 v 456 W 774 w 638 X 547 x 456 Y 547 y 456 Yacute 547 yacute 456 Ydieresis 547 ydieresis 456 yen 456 Z 501 z 410 Zcaron 501 zcaron 410 zero 456}
set font_widths(Helvetica-Narrow-Oblique) {A 547 a 456 Aacute 547 aacute 456 Acircumflex 547 acircumflex 456 acute 273 Adieresis 547 adieresis 456 AE 820 ae 729 Agrave 547 agrave 456 ampersand 547 Aring 547 aring 456 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 385 asciitilde 479 asterisk 319 at 832 Atilde 547 atilde 456 B 547 b 456 backslash 228 bar 213 braceleft 274 braceright 274 bracketleft 228 bracketright 228 breve 273 brokenbar 213 bullet 287 C 592 c 410 caron 273 Ccedilla 592 ccedilla 410 cedilla 273 cent 456 center 600 circumflex 273 colon 228 comma 228 copyright 604 currency 456 D 592 d 456 dagger 456 daggerdbl 456 dectab 600 degree 328 dieresis 273 divide 479 dollar 456 dotaccent 273 dotlessi 228 down 600 E 547 e 456 Eacute 547 eacute 456 Ecircumflex 547 ecircumflex 456 Edieresis 547 edieresis 456 Egrave 547 egrave 456 eight 456 ellipsis 820 emdash 820 endash 456 equal 479 Eth 592 eth 456 exclam 228 exclamdown 273 F 501 f 228 fi 410 five 456 fl 410 florin 456 format 600 four 456 fraction 137 G 638 g 456 Gcaron 600 gcaron 600 germandbls 501 grave 273 graybox 600 greater 479 guillemotleft 456 guillemotright 456 guilsinglleft 273 guilsinglright 273 H 592 h 456 hungarumlaut 273 hyphen 273 I 228 i 182 Iacute 228 iacute 228 Icircumflex 228 icircumflex 228 Idieresis 228 idieresis 228 Idot 600 Igrave 228 igrave 228 IJ 600 ij 600 indent 600 J 410 j 182 K 547 k 410 L 456 l 182 largebullet 600 left 600 less 479 lira 600 LL 600 ll 600 logicalnot 479 Lslash 456 lslash 182 M 683 m 683 macron 273 merge 600 minus 479 mu 456 multiply 479 N 592 n 456 nine 456 notegraphic 600 Ntilde 592 ntilde 456 numbersign 456 O 638 o 456 Oacute 638 oacute 456 Ocircumflex 638 ocircumflex 456 Odieresis 638 odieresis 456 OE 820 oe 774 ogonek 273 Ograve 638 ograve 456 one 456 onehalf 684 onequarter 684 onesuperior 273 ordfeminine 303 ordmasculine 299 Oslash 638 oslash 501 Otilde 638 otilde 456 overscore 600 P 547 p 456 paragraph 440 parenleft 273 parenright 273 percent 729 period 228 periodcentered 228 perthousand 820 plus 479 plusminus 479 prescription 600 Q 638 q 456 question 456 questiondown 501 quotedbl 291 quotedblbase 273 quotedblleft 273 quotedblright 273 quoteleft 182 quoteright 182 quotesinglbase 182 quotesingle 157 R 592 r 273 registered 604 return 600 ring 273 S 547 s 410 Scaron 547 scaron 410 Scedilla 600 scedilla 600 section 456 semicolon 228 seven 456 six 456 slash 228 space 228 square 600 sterling 456 stop 600 T 501 t 228 tab 600 Thorn 547 thorn 456 three 456 threequarters 684 threesuperior 273 tilde 273 trademark 820 two 456 twosuperior 273 U 592 u 456 Uacute 592 uacute 456 Ucircumflex 592 ucircumflex 456 Udieresis 592 udieresis 456 Ugrave 592 ugrave 456 underscore 456 up 600 V 547 v 410 W 774 w 592 X 547 x 410 Y 547 y 410 Yacute 547 yacute 410 Ydieresis 547 ydieresis 410 yen 456 Z 501 z 410 Zcaron 501 zcaron 410 zero 456}
set font_widths(Helvetica-Oblique) {A 667 a 556 Aacute 667 aacute 556 Abreve 667 abreve 556 Acircumflex 667 acircumflex 556 acute 333 Adieresis 667 adieresis 556 AE 1000 ae 889 Agrave 667 agrave 556 Amacron 667 amacron 556 ampersand 667 Aogonek 667 aogonek 556 Aring 667 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 469 asciitilde 584 asterisk 389 at 1015 Atilde 667 atilde 556 B 667 b 556 backslash 278 bar 260 braceleft 334 braceright 334 bracketleft 278 bracketright 278 breve 333 brokenbar 260 bullet 350 C 722 c 500 Cacute 722 cacute 500 caron 333 Ccaron 722 ccaron 500 Ccedilla 722 ccedilla 500 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 commaaccent 250 copyright 737 currency 556 D 722 d 556 dagger 556 daggerdbl 556 Dcaron 722 dcaron 643 Dcroat 722 dcroat 556 dectab 600 degree 400 Delta 612 dieresis 333 divide 584 dollar 556 dotaccent 333 dotlessi 278 down 600 E 667 e 556 Eacute 667 eacute 556 Ecaron 667 ecaron 556 Ecircumflex 667 ecircumflex 556 Edieresis 667 edieresis 556 Edotaccent 667 edotaccent 556 Egrave 667 egrave 556 eight 556 ellipsis 1000 Emacron 667 emacron 556 emdash 1000 endash 556 Eogonek 667 eogonek 556 equal 584 Eth 722 eth 556 Euro 556 exclam 278 exclamdown 333 F 611 f 278 fi 500 five 556 fl 500 florin 556 format 600 four 556 fraction 167 G 778 g 556 Gbreve 778 gbreve 556 Gcaron 600 gcaron 600 Gcommaaccent 778 gcommaaccent 556 germandbls 611 grave 333 graybox 600 greater 584 greaterequal 549 guillemotleft 556 guillemotright 556 guilsinglleft 333 guilsinglright 333 H 722 h 556 hungarumlaut 333 hyphen 333 I 278 i 222 Iacute 278 iacute 278 Icircumflex 278 icircumflex 278 Idieresis 278 idieresis 278 Idot 600 Idotaccent 278 Igrave 278 igrave 278 IJ 600 ij 600 Imacron 278 imacron 278 indent 600 Iogonek 278 iogonek 222 J 500 j 222 K 667 k 500 Kcommaaccent 667 kcommaaccent 500 L 556 l 222 Lacute 556 lacute 222 largebullet 600 Lcaron 556 lcaron 299 Lcommaaccent 556 lcommaaccent 222 left 600 less 584 lessequal 549 lira 600 LL 600 ll 600 logicalnot 584 lozenge 471 Lslash 556 lslash 222 M 833 m 833 macron 333 merge 600 minus 584 mu 556 multiply 584 N 722 n 556 Nacute 722 nacute 556 Ncaron 722 ncaron 556 Ncommaaccent 722 ncommaaccent 556 nine 556 notegraphic 600 notequal 549 Ntilde 722 ntilde 556 numbersign 556 O 778 o 556 Oacute 778 oacute 556 Ocircumflex 778 ocircumflex 556 Odieresis 778 odieresis 556 OE 1000 oe 944 ogonek 333 Ograve 778 ograve 556 Ohungarumlaut 778 ohungarumlaut 556 Omacron 778 omacron 556 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 370 ordmasculine 365 Oslash 778 oslash 611 Otilde 778 otilde 556 overscore 600 P 667 p 556 paragraph 537 parenleft 333 parenright 333 partialdiff 476 percent 889 period 278 periodcentered 278 perthousand 1000 plus 584 plusminus 584 prescription 600 Q 778 q 556 question 556 questiondown 611 quotedbl 355 quotedblbase 333 quotedblleft 333 quotedblright 333 quoteleft 222 quoteright 222 quotesinglbase 222 quotesingle 191 R 722 r 333 Racute 722 racute 333 radical 453 Rcaron 722 rcaron 333 Rcommaaccent 722 rcommaaccent 333 registered 737 return 600 ring 333 S 667 s 500 Sacute 667 sacute 500 Scaron 667 scaron 500 Scedilla 667 scedilla 500 Scommaaccent 667 scommaaccent 500 section 556 semicolon 278 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 summation 600 T 611 t 278 tab 600 Tcaron 611 tcaron 317 Tcommaaccent 611 tcommaaccent 278 Thorn 667 thorn 556 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 1000 two 556 twosuperior 333 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 Uhungarumlaut 722 uhungarumlaut 556 Umacron 722 umacron 556 underscore 556 Uogonek 722 uogonek 556 up 600 Uring 722 uring 556 V 667 v 500 W 944 w 722 X 667 x 500 Y 667 y 500 Yacute 667 yacute 500 Ydieresis 667 ydieresis 500 yen 556 Z 611 z 500 Zacute 611 zacute 500 Zcaron 611 zcaron 500 Zdotaccent 611 zdotaccent 500 zero 556}
set font_widths(NewCenturySchlbk-Bold) {A 759 a 611 Aacute 759 aacute 611 Acircumflex 759 acircumflex 611 acute 333 Adieresis 759 adieresis 611 AE 981 ae 870 Agrave 759 agrave 611 ampersand 852 Aring 759 aring 611 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 500 at 747 Atilde 759 atilde 611 B 778 b 648 backslash 606 bar 606 braceleft 389 braceright 389 bracketleft 389 bracketright 389 breve 333 brokenbar 606 bullet 606 C 778 c 556 caron 333 Ccedilla 778 ccedilla 556 cedilla 333 cent 574 center 600 circumflex 333 colon 278 comma 278 copyright 747 currency 574 D 833 d 667 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 574 dotaccent 333 dotlessi 370 down 600 E 759 e 574 Eacute 759 eacute 574 Ecircumflex 759 ecircumflex 574 Edieresis 759 edieresis 574 Egrave 759 egrave 574 eight 574 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 833 eth 611 exclam 296 exclamdown 296 F 722 f 389 fi 685 five 574 fl 685 florin 574 format 600 four 574 fraction 167 G 833 g 611 Gcaron 600 gcaron 600 germandbls 611 grave 333 graybox 600 greater 606 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 870 h 685 hungarumlaut 333 hyphen 333 I 444 i 370 Iacute 444 iacute 370 Icircumflex 444 icircumflex 370 Idieresis 444 idieresis 370 Idot 600 Igrave 444 igrave 370 IJ 600 ij 600 indent 600 J 648 j 352 K 815 k 667 L 722 l 352 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 722 lslash 352 M 981 m 963 macron 333 merge 600 minus 606 mu 685 multiply 606 N 833 n 685 nine 574 notegraphic 600 Ntilde 833 ntilde 685 numbersign 574 O 833 o 611 Oacute 833 oacute 611 Ocircumflex 833 ocircumflex 611 Odieresis 833 odieresis 611 OE 1000 oe 907 ogonek 333 Ograve 833 ograve 611 one 574 onehalf 861 onequarter 861 onesuperior 344 ordfeminine 367 ordmasculine 367 Oslash 833 oslash 611 Otilde 833 otilde 611 overscore 600 P 759 p 667 paragraph 747 parenleft 389 parenright 389 percent 833 period 278 periodcentered 278 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 833 q 648 question 500 questiondown 500 quotedbl 333 quotedblbase 481 quotedblleft 481 quotedblright 481 quoteleft 241 quoteright 241 quotesinglbase 241 quotesingle 241 R 815 r 519 registered 747 return 600 ring 333 S 667 s 500 Scaron 667 scaron 500 Scedilla 600 scedilla 600 section 500 semicolon 278 seven 574 six 574 slash 278 space 287 square 600 sterling 574 stop 600 T 722 t 426 tab 600 Thorn 759 thorn 667 three 574 threequarters 861 threesuperior 344 tilde 333 trademark 1000 two 574 twosuperior 344 U 833 u 685 Uacute 833 uacute 685 Ucircumflex 833 ucircumflex 685 Udieresis 833 udieresis 685 Ugrave 833 ugrave 685 underscore 500 up 600 V 759 v 611 W 981 w 889 X 722 x 611 Y 722 y 611 Yacute 722 yacute 611 Ydieresis 722 ydieresis 611 yen 574 Z 667 z 537 Zcaron 667 zcaron 537 zero 574}
set font_widths(NewCenturySchlbk-BoldItalic) {A 741 a 667 Aacute 741 aacute 667 Acircumflex 741 acircumflex 667 acute 333 Adieresis 741 adieresis 667 AE 889 ae 815 Agrave 741 agrave 667 ampersand 889 Aring 741 aring 667 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 500 at 747 Atilde 741 atilde 667 B 759 b 611 backslash 606 bar 606 braceleft 407 braceright 407 bracketleft 407 bracketright 407 breve 333 brokenbar 606 bullet 606 C 759 c 537 caron 333 Ccedilla 759 ccedilla 537 cedilla 333 cent 574 center 600 circumflex 333 colon 287 comma 287 copyright 747 currency 574 D 833 d 667 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 574 dotaccent 333 dotlessi 389 down 600 E 741 e 519 Eacute 741 eacute 519 Ecircumflex 741 ecircumflex 519 Edieresis 741 edieresis 519 Egrave 741 egrave 519 eight 574 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 833 eth 574 exclam 333 exclamdown 333 F 704 f 389 fi 685 five 574 fl 685 florin 574 format 600 four 574 fraction 167 G 815 g 611 Gcaron 600 gcaron 600 germandbls 574 grave 333 graybox 600 greater 606 guillemotleft 481 guillemotright 481 guilsinglleft 278 guilsinglright 278 H 870 h 685 hungarumlaut 333 hyphen 333 I 444 i 389 Iacute 444 iacute 389 Icircumflex 444 icircumflex 389 Idieresis 444 idieresis 389 Idot 600 Igrave 444 igrave 389 IJ 600 ij 600 indent 600 J 667 j 370 K 778 k 648 L 704 l 389 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 704 lslash 389 M 944 m 944 macron 333 merge 600 minus 606 mu 685 multiply 606 N 852 n 685 nine 574 notegraphic 600 Ntilde 852 ntilde 685 numbersign 574 O 833 o 574 Oacute 833 oacute 574 Ocircumflex 833 ocircumflex 574 Odieresis 833 odieresis 574 OE 963 oe 852 ogonek 333 Ograve 833 ograve 574 one 574 onehalf 861 onequarter 861 onesuperior 344 ordfeminine 412 ordmasculine 356 Oslash 833 oslash 574 Otilde 833 otilde 574 overscore 600 P 741 p 648 paragraph 650 parenleft 407 parenright 407 percent 889 period 287 periodcentered 287 perthousand 1167 plus 606 plusminus 606 prescription 600 Q 833 q 630 question 481 questiondown 481 quotedbl 400 quotedblbase 481 quotedblleft 481 quotedblright 481 quoteleft 259 quoteright 259 quotesinglbase 259 quotesingle 287 R 796 r 519 registered 747 return 600 ring 333 S 685 s 481 Scaron 685 scaron 481 Scedilla 600 scedilla 600 section 500 semicolon 287 seven 574 six 574 slash 278 space 287 square 600 sterling 574 stop 600 T 722 t 407 tab 600 Thorn 741 thorn 648 three 574 threequarters 861 threesuperior 344 tilde 333 trademark 950 two 574 twosuperior 344 U 833 u 685 Uacute 833 uacute 685 Ucircumflex 833 ucircumflex 685 Udieresis 833 udieresis 685 Ugrave 833 ugrave 685 underscore 500 up 600 V 741 v 556 W 944 w 833 X 741 x 574 Y 704 y 519 Yacute 704 yacute 519 Ydieresis 704 ydieresis 519 yen 574 Z 704 z 519 Zcaron 704 zcaron 519 zero 574}
set font_widths(NewCenturySchlbk-Italic) {A 704 a 574 Aacute 704 aacute 574 Acircumflex 704 acircumflex 574 acute 333 Adieresis 704 adieresis 574 AE 870 ae 722 Agrave 704 agrave 574 ampersand 852 Aring 704 aring 574 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 500 at 747 Atilde 704 atilde 574 B 722 b 556 backslash 606 bar 606 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 606 C 722 c 444 caron 333 Ccedilla 722 ccedilla 444 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 copyright 747 currency 556 D 778 d 611 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 556 dotaccent 333 dotlessi 333 down 600 E 722 e 444 Eacute 722 eacute 444 Ecircumflex 722 ecircumflex 444 Edieresis 722 edieresis 444 Egrave 722 egrave 444 eight 556 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 778 eth 500 exclam 333 exclamdown 333 F 667 f 333 fi 611 five 556 fl 611 florin 556 format 600 four 556 fraction 167 G 778 g 537 Gcaron 600 gcaron 600 germandbls 556 grave 333 graybox 600 greater 606 guillemotleft 426 guillemotright 426 guilsinglleft 333 guilsinglright 333 H 833 h 611 hungarumlaut 333 hyphen 333 I 407 i 333 Iacute 407 iacute 333 Icircumflex 407 icircumflex 333 Idieresis 407 idieresis 333 Idot 600 Igrave 407 igrave 333 IJ 600 ij 600 indent 600 J 611 j 315 K 741 k 556 L 667 l 333 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 667 lslash 333 M 944 m 889 macron 333 merge 600 minus 606 mu 611 multiply 606 N 815 n 611 nine 556 notegraphic 600 Ntilde 815 ntilde 611 numbersign 556 O 778 o 500 Oacute 778 oacute 500 Ocircumflex 778 ocircumflex 500 Odieresis 778 odieresis 500 OE 981 oe 778 ogonek 333 Ograve 778 ograve 500 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 422 ordmasculine 372 Oslash 778 oslash 500 Otilde 778 otilde 500 overscore 600 P 667 p 574 paragraph 650 parenleft 333 parenright 333 percent 833 period 278 periodcentered 278 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 778 q 556 question 444 questiondown 444 quotedbl 400 quotedblbase 389 quotedblleft 389 quotedblright 389 quoteleft 204 quoteright 204 quotesinglbase 204 quotesingle 278 R 741 r 444 registered 747 return 600 ring 333 S 667 s 444 Scaron 667 scaron 444 Scedilla 600 scedilla 600 section 500 semicolon 278 seven 556 six 556 slash 606 space 278 square 600 sterling 556 stop 600 T 685 t 352 tab 600 Thorn 667 thorn 574 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 950 two 556 twosuperior 333 U 815 u 611 Uacute 815 uacute 611 Ucircumflex 815 ucircumflex 611 Udieresis 815 udieresis 611 Ugrave 815 ugrave 611 underscore 500 up 600 V 704 v 519 W 926 w 778 X 704 x 500 Y 685 y 500 Yacute 685 yacute 500 Ydieresis 685 ydieresis 500 yen 556 Z 667 z 463 Zcaron 667 zcaron 463 zero 556}
set font_widths(NewCenturySchlbk-Roman) {A 722 a 556 Aacute 722 aacute 556 Acircumflex 722 acircumflex 556 acute 333 Adieresis 722 adieresis 556 AE 1000 ae 796 Agrave 722 agrave 556 ampersand 815 Aring 722 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 500 at 737 Atilde 722 atilde 556 B 722 b 556 backslash 606 bar 606 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 606 C 722 c 444 caron 333 Ccedilla 722 ccedilla 444 cedilla 333 cent 556 center 600 circumflex 333 colon 278 comma 278 copyright 737 currency 556 D 778 d 574 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 556 dotaccent 333 dotlessi 315 down 600 E 722 e 500 Eacute 722 eacute 500 Ecircumflex 722 ecircumflex 500 Edieresis 722 edieresis 500 Egrave 722 egrave 500 eight 556 ellipsis 1000 emdash 1000 endash 556 equal 606 Eth 778 eth 500 exclam 296 exclamdown 296 F 667 f 333 fi 611 five 556 fl 611 florin 556 format 600 four 556 fraction 167 G 778 g 537 Gcaron 600 gcaron 600 germandbls 574 grave 333 graybox 600 greater 606 guillemotleft 426 guillemotright 426 guilsinglleft 259 guilsinglright 259 H 833 h 611 hungarumlaut 333 hyphen 333 I 407 i 315 Iacute 407 iacute 315 Icircumflex 407 icircumflex 315 Idieresis 407 idieresis 315 Idot 600 Igrave 407 igrave 315 IJ 600 ij 600 indent 600 J 556 j 296 K 778 k 593 L 667 l 315 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 667 lslash 315 M 944 m 889 macron 333 merge 600 minus 606 mu 611 multiply 606 N 815 n 611 nine 556 notegraphic 600 Ntilde 815 ntilde 611 numbersign 556 O 778 o 500 Oacute 778 oacute 500 Ocircumflex 778 ocircumflex 500 Odieresis 778 odieresis 500 OE 1000 oe 833 ogonek 333 Ograve 778 ograve 500 one 556 onehalf 834 onequarter 834 onesuperior 333 ordfeminine 334 ordmasculine 300 Oslash 778 oslash 500 Otilde 778 otilde 500 overscore 600 P 667 p 574 paragraph 606 parenleft 333 parenright 333 percent 833 period 278 periodcentered 278 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 778 q 556 question 444 questiondown 444 quotedbl 389 quotedblbase 389 quotedblleft 389 quotedblright 389 quoteleft 204 quoteright 204 quotesinglbase 204 quotesingle 204 R 722 r 444 registered 737 return 600 ring 333 S 630 s 463 Scaron 630 scaron 463 Scedilla 600 scedilla 600 section 500 semicolon 278 seven 556 six 556 slash 278 space 278 square 600 sterling 556 stop 600 T 667 t 389 tab 600 Thorn 667 thorn 574 three 556 threequarters 834 threesuperior 333 tilde 333 trademark 1000 two 556 twosuperior 333 U 815 u 611 Uacute 815 uacute 611 Ucircumflex 815 ucircumflex 611 Udieresis 815 udieresis 611 Ugrave 815 ugrave 611 underscore 500 up 600 V 722 v 537 W 981 w 778 X 704 x 537 Y 704 y 537 Yacute 704 yacute 537 Ydieresis 704 ydieresis 537 yen 556 Z 611 z 481 Zcaron 611 zcaron 481 zero 556}
set font_widths(Palatino-Bold) {A 778 a 500 Aacute 778 aacute 500 Acircumflex 778 acircumflex 500 acute 333 Adieresis 778 adieresis 500 AE 1000 ae 778 Agrave 778 agrave 500 ampersand 833 Aring 778 aring 500 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 444 at 747 Atilde 778 atilde 500 B 667 b 611 backslash 606 bar 606 braceleft 310 braceright 310 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 606 C 722 c 444 caron 333 Ccedilla 722 ccedilla 444 cedilla 333 cent 500 center 600 circumflex 333 colon 250 comma 250 copyright 747 currency 500 D 833 d 611 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 500 dotaccent 333 dotlessi 333 down 600 E 611 e 500 Eacute 611 eacute 500 Ecircumflex 611 ecircumflex 500 Edieresis 611 edieresis 500 Egrave 611 egrave 500 eight 500 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 833 eth 556 exclam 278 exclamdown 278 F 556 f 389 fi 611 five 500 fl 611 florin 500 format 600 four 500 fraction 167 G 833 g 556 Gcaron 600 gcaron 600 germandbls 611 grave 333 graybox 600 greater 606 guillemotleft 500 guillemotright 500 guilsinglleft 389 guilsinglright 389 H 833 h 611 hungarumlaut 333 hyphen 333 I 389 i 333 Iacute 389 iacute 333 Icircumflex 389 icircumflex 333 Idieresis 389 idieresis 333 Idot 600 Igrave 389 igrave 333 IJ 600 ij 600 indent 600 J 389 j 333 K 778 k 611 L 611 l 333 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 611 lslash 333 M 1000 m 889 macron 333 merge 600 minus 606 mu 611 multiply 606 N 833 n 611 nine 500 notegraphic 600 Ntilde 833 ntilde 611 numbersign 500 O 833 o 556 Oacute 833 oacute 556 Ocircumflex 833 ocircumflex 556 Odieresis 833 odieresis 556 OE 1000 oe 833 ogonek 333 Ograve 833 ograve 556 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 438 ordmasculine 488 Oslash 833 oslash 556 Otilde 833 otilde 556 overscore 600 P 611 p 611 paragraph 641 parenleft 333 parenright 333 percent 889 period 250 periodcentered 250 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 833 q 611 question 444 questiondown 444 quotedbl 402 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 333 quotesingle 227 R 722 r 389 registered 747 return 600 ring 333 S 611 s 444 Scaron 611 scaron 444 Scedilla 600 scedilla 600 section 500 semicolon 250 seven 500 six 500 slash 296 space 250 square 600 sterling 500 stop 600 T 667 t 333 tab 600 Thorn 611 thorn 611 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 998 two 500 twosuperior 300 U 778 u 611 Uacute 778 uacute 611 Ucircumflex 778 ucircumflex 611 Udieresis 778 udieresis 611 Ugrave 778 ugrave 611 underscore 500 up 600 V 778 v 556 W 1000 w 833 X 667 x 500 Y 667 y 556 Yacute 667 yacute 556 Ydieresis 667 ydieresis 556 yen 500 Z 667 z 500 Zcaron 667 zcaron 500 zero 500}
set font_widths(Palatino-BoldItalic) {A 722 a 556 Aacute 722 aacute 556 Acircumflex 722 acircumflex 556 acute 333 Adieresis 722 adieresis 556 AE 944 ae 738 Agrave 722 agrave 556 ampersand 833 Aring 722 aring 556 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 444 at 833 Atilde 722 atilde 556 B 667 b 537 backslash 606 bar 606 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 606 C 685 c 444 caron 333 Ccedilla 685 ccedilla 444 cedilla 333 cent 500 center 600 circumflex 333 colon 250 comma 250 copyright 747 currency 500 D 778 d 556 dagger 556 daggerdbl 556 dectab 600 degree 400 dieresis 333 divide 606 dollar 500 dotaccent 333 dotlessi 333 down 600 E 611 e 444 Eacute 611 eacute 444 Ecircumflex 611 ecircumflex 444 Edieresis 611 edieresis 444 Egrave 611 egrave 444 eight 500 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 778 eth 556 exclam 333 exclamdown 333 F 556 f 333 fi 611 five 500 fl 611 florin 500 format 600 four 500 fraction 167 G 778 g 500 Gcaron 600 gcaron 600 germandbls 556 grave 333 graybox 600 greater 606 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 778 h 556 hungarumlaut 333 hyphen 389 I 389 i 333 Iacute 389 iacute 333 Icircumflex 389 icircumflex 333 Idieresis 389 idieresis 333 Idot 600 Igrave 389 igrave 333 IJ 600 ij 600 indent 600 J 389 j 333 K 722 k 556 L 611 l 333 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 611 lslash 333 M 944 m 833 macron 333 merge 600 minus 606 mu 556 multiply 606 N 778 n 556 nine 500 notegraphic 600 Ntilde 778 ntilde 556 numbersign 500 O 833 o 556 Oacute 833 oacute 556 Ocircumflex 833 ocircumflex 556 Odieresis 833 odieresis 556 OE 944 oe 778 ogonek 333 Ograve 833 ograve 556 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 333 ordmasculine 333 Oslash 833 oslash 556 Otilde 833 otilde 556 overscore 600 P 667 p 556 paragraph 556 parenleft 333 parenright 333 percent 889 period 250 periodcentered 250 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 833 q 537 question 444 questiondown 444 quotedbl 500 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 250 quotesingle 250 R 722 r 389 registered 747 return 600 ring 556 S 556 s 444 Scaron 556 scaron 444 Scedilla 600 scedilla 600 section 556 semicolon 250 seven 500 six 500 slash 315 space 250 square 600 sterling 500 stop 600 T 611 t 389 tab 600 Thorn 667 thorn 556 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 1000 two 500 twosuperior 300 U 778 u 556 Uacute 778 uacute 556 Ucircumflex 778 ucircumflex 556 Udieresis 778 udieresis 556 Ugrave 778 ugrave 556 underscore 500 up 600 V 667 v 556 W 1000 w 833 X 722 x 500 Y 611 y 556 Yacute 611 yacute 556 Ydieresis 611 ydieresis 556 yen 500 Z 667 z 500 Zcaron 667 zcaron 500 zero 500}
set font_widths(Palatino-Italic) {A 722 a 444 Aacute 722 aacute 444 Acircumflex 722 acircumflex 444 acute 333 Adieresis 722 adieresis 444 AE 941 ae 638 Agrave 722 agrave 444 ampersand 778 Aring 722 aring 444 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 389 at 747 Atilde 722 atilde 444 B 611 b 463 backslash 606 bar 606 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 500 C 667 c 407 caron 333 Ccedilla 667 ccedilla 407 cedilla 333 cent 500 center 600 circumflex 333 colon 250 comma 250 copyright 747 currency 500 D 778 d 500 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 500 dotaccent 333 dotlessi 278 down 600 E 611 e 389 Eacute 611 eacute 389 Ecircumflex 611 ecircumflex 389 Edieresis 611 edieresis 389 Egrave 611 egrave 389 eight 500 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 778 eth 444 exclam 333 exclamdown 333 F 556 f 278 fi 528 five 500 fl 545 florin 500 format 600 four 500 fraction 167 G 722 g 500 Gcaron 600 gcaron 600 germandbls 500 grave 333 graybox 600 greater 606 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 778 h 500 hungarumlaut 333 hyphen 333 I 333 i 278 Iacute 333 iacute 278 Icircumflex 333 icircumflex 278 Idieresis 333 idieresis 278 Idot 600 Igrave 333 igrave 278 IJ 600 ij 600 indent 600 J 333 j 278 K 667 k 444 L 556 l 278 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 556 lslash 278 M 944 m 778 macron 333 merge 600 minus 606 mu 556 multiply 606 N 778 n 556 nine 500 notegraphic 600 Ntilde 778 ntilde 556 numbersign 500 O 778 o 444 Oacute 778 oacute 444 Ocircumflex 778 ocircumflex 444 Odieresis 778 odieresis 444 OE 1028 oe 669 ogonek 333 Ograve 778 ograve 444 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 333 ordmasculine 333 Oslash 778 oslash 444 Otilde 778 otilde 444 overscore 600 P 611 p 500 paragraph 500 parenleft 333 parenright 333 percent 889 period 250 periodcentered 250 perthousand 1000 plus 606 plusminus 606 prescription 600 Q 778 q 463 question 500 questiondown 500 quotedbl 500 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 333 R 667 r 389 registered 747 return 600 ring 333 S 556 s 389 Scaron 556 scaron 389 Scedilla 600 scedilla 600 section 500 semicolon 250 seven 500 six 500 slash 296 space 250 square 600 sterling 500 stop 600 T 611 t 333 tab 600 Thorn 611 thorn 500 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 1000 two 500 twosuperior 300 U 778 u 556 Uacute 778 uacute 556 Ucircumflex 778 ucircumflex 556 Udieresis 778 udieresis 556 Ugrave 778 ugrave 556 underscore 500 up 600 V 722 v 500 W 944 w 722 X 722 x 500 Y 667 y 500 Yacute 667 yacute 500 Ydieresis 667 ydieresis 500 yen 500 Z 667 z 444 Zcaron 667 zcaron 444 zero 500}
set font_widths(Palatino-Roman) {A 778 a 500 Aacute 778 aacute 500 Acircumflex 778 acircumflex 500 acute 333 Adieresis 778 adieresis 500 AE 944 ae 758 Agrave 778 agrave 500 ampersand 778 Aring 778 aring 500 arrowboth 600 arrowdown 600 arrowleft 600 arrowright 600 arrowup 600 asciicircum 606 asciitilde 606 asterisk 389 at 747 Atilde 778 atilde 500 B 611 b 553 backslash 606 bar 606 braceleft 333 braceright 333 bracketleft 333 bracketright 333 breve 333 brokenbar 606 bullet 606 C 709 c 444 caron 333 Ccedilla 709 ccedilla 444 cedilla 333 cent 500 center 600 circumflex 333 colon 250 comma 250 copyright 747 currency 500 D 774 d 611 dagger 500 daggerdbl 500 dectab 600 degree 400 dieresis 333 divide 606 dollar 500 dotaccent 250 dotlessi 287 down 600 E 611 e 479 Eacute 611 eacute 479 Ecircumflex 611 ecircumflex 479 Edieresis 611 edieresis 479 Egrave 611 egrave 479 eight 500 ellipsis 1000 emdash 1000 endash 500 equal 606 Eth 774 eth 546 exclam 278 exclamdown 278 F 556 f 333 fi 605 five 500 fl 608 florin 500 format 600 four 500 fraction 167 G 763 g 556 Gcaron 600 gcaron 600 germandbls 556 grave 333 graybox 600 greater 606 guillemotleft 500 guillemotright 500 guilsinglleft 331 guilsinglright 331 H 832 h 582 hungarumlaut 380 hyphen 333 I 337 i 291 Iacute 337 iacute 287 Icircumflex 337 icircumflex 287 Idieresis 337 idieresis 287 Idot 600 Igrave 337 igrave 287 IJ 600 ij 600 indent 600 J 333 j 234 K 726 k 556 L 611 l 291 largebullet 600 left 600 less 606 lira 600 LL 600 ll 600 logicalnot 606 Lslash 611 lslash 291 M 946 m 883 macron 333 merge 600 minus 606 mu 603 multiply 606 N 831 n 582 nine 500 notegraphic 600 Ntilde 831 ntilde 582 numbersign 500 O 786 o 546 Oacute 786 oacute 546 Ocircumflex 786 ocircumflex 546 Odieresis 786 odieresis 546 OE 998 oe 827 ogonek 313 Ograve 786 ograve 546 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 333 ordmasculine 333 Oslash 833 oslash 556 Otilde 786 otilde 546 overscore 600 P 604 p 601 paragraph 628 parenleft 333 parenright 333 percent 840 period 250 periodcentered 250 perthousand 1144 plus 606 plusminus 606 prescription 600 Q 786 q 560 question 444 questiondown 444 quotedbl 371 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 208 R 668 r 395 registered 747 return 600 ring 333 S 525 s 424 Scaron 525 scaron 424 Scedilla 600 scedilla 600 section 500 semicolon 250 seven 500 six 500 slash 606 space 250 square 600 sterling 500 stop 600 T 613 t 326 tab 600 Thorn 604 thorn 601 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 979 two 500 twosuperior 300 U 778 u 603 Uacute 778 uacute 603 Ucircumflex 778 ucircumflex 603 Udieresis 778 udieresis 603 Ugrave 778 ugrave 603 underscore 500 up 600 V 722 v 565 W 1000 w 834 X 667 x 516 Y 667 y 556 Yacute 667 yacute 556 Ydieresis 667 ydieresis 556 yen 500 Z 667 z 500 Zcaron 667 zcaron 500 zero 500}
set font_widths(Symbol) {A 722 a 444 Aacute 722 aacute 444 Acircumflex 722 acircumflex 444 acute 333 Adieresis 722 adieresis 444 AE 941 ae 638 Agrave 722 agrave 444 aleph 823 Alpha 722 alpha 631 ampersand 778 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 722 aring 444 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 606 asciitilde 606 asterisk 389 asteriskmath 500 at 747 Atilde 722 atilde 444 B 611 b 463 backslash 606 bar 200 Beta 667 beta 549 braceex 494 braceleft 480 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 480 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 333 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 333 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 333 brokenbar 606 bullet 460 C 667 c 407 caron 333 carriagereturn 658 Ccedilla 667 ccedilla 407 cedilla 333 cent 500 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 333 club 753 colon 278 comma 250 congruent 549 copyright 747 copyrightsans 790 copyrightserif 790 currency 500 D 778 d 500 dagger 500 daggerdbl 500 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 333 divide 549 dollar 500 dotaccent 333 dotlessi 278 dotmath 250 down 600 E 611 e 389 Eacute 611 eacute 389 Ecircumflex 611 ecircumflex 389 Edieresis 611 edieresis 389 Egrave 611 egrave 389 eight 500 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 549 equivalence 549 Eta 722 eta 603 Eth 778 eth 444 Euro 750 exclam 333 exclamdown 333 existential 549 F 556 f 278 fi 528 five 500 fl 545 florin 500 format 600 four 500 fraction 167 G 722 g 500 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 500 gradient 713 grave 333 graybox 600 greater 549 greaterequal 549 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 778 h 500 heart 753 hungarumlaut 333 hyphen 333 I 333 i 278 Iacute 333 iacute 278 Icircumflex 333 icircumflex 278 Idieresis 333 idieresis 278 Idot 600 Ifraktur 686 Igrave 333 igrave 278 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 333 j 278 K 667 k 444 Kappa 722 kappa 549 L 556 l 278 Lambda 686 lambda 549 largebullet 600 left 600 less 549 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 713 logicalor 603 lozenge 494 Lslash 556 lslash 278 M 944 m 778 macron 333 merge 600 minus 549 minute 247 Mu 889 mu 576 multiply 549 N 778 n 556 nine 500 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 778 ntilde 556 Nu 722 nu 521 numbersign 500 O 778 o 444 Oacute 778 oacute 444 Ocircumflex 778 ocircumflex 444 Odieresis 778 odieresis 444 OE 1028 oe 669 ogonek 333 Ograve 778 ograve 444 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 333 ordmasculine 333 Oslash 778 oslash 444 Otilde 778 otilde 444 overscore 600 P 611 p 500 paragraph 500 parenleft 333 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 333 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 833 period 250 periodcentered 250 perpendicular 658 perthousand 1000 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 549 plusminus 549 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 778 q 463 question 444 questiondown 500 quotedbl 500 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 333 R 667 r 389 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 747 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 333 S 556 s 389 Scaron 556 scaron 389 Scedilla 600 scedilla 600 second 411 section 500 semicolon 278 seven 500 Sigma 592 sigma 603 sigma1 439 similar 549 six 500 slash 278 space 250 spade 753 square 600 sterling 500 stop 600 suchthat 439 summation 713 T 611 t 333 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 611 thorn 500 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 1000 trademarksans 786 trademarkserif 890 two 500 twosuperior 300 U 778 u 556 Uacute 778 uacute 556 Ucircumflex 778 ucircumflex 556 Udieresis 778 udieresis 556 Ugrave 778 ugrave 556 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 722 v 500 W 944 w 722 weierstrass 987 X 722 x 500 Xi 645 xi 493 Y 667 y 500 Yacute 667 yacute 500 Ydieresis 667 ydieresis 500 yen 500 Z 667 z 444 Zcaron 667 zcaron 444 zero 500 Zeta 611 zeta 494}
set font_widths(Times-Bold) {A 722 a 500 Aacute 722 aacute 500 Abreve 722 abreve 500 Acircumflex 722 acircumflex 500 acute 333 Adieresis 722 adieresis 500 AE 1000 ae 722 Agrave 722 agrave 500 aleph 823 Alpha 722 alpha 631 Amacron 722 amacron 500 ampersand 833 angle 768 angleleft 329 angleright 329 Aogonek 722 aogonek 500 apple 790 approxequal 549 Aring 722 aring 500 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 581 asciitilde 520 asterisk 500 asteriskmath 500 at 930 Atilde 722 atilde 500 B 667 b 556 backslash 278 bar 220 Beta 667 beta 549 braceex 494 braceleft 394 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 394 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 333 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 333 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 333 brokenbar 220 bullet 350 C 722 c 444 Cacute 722 cacute 444 caron 333 carriagereturn 658 Ccaron 722 ccaron 444 Ccedilla 722 ccedilla 444 cedilla 333 cent 500 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 333 club 753 colon 333 comma 250 commaaccent 250 congruent 549 copyright 747 copyrightsans 790 copyrightserif 790 currency 500 D 722 d 556 dagger 500 daggerdbl 500 Dcaron 722 dcaron 672 Dcroat 722 dcroat 556 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 333 divide 570 dollar 500 dotaccent 333 dotlessi 278 dotmath 250 down 600 E 667 e 444 Eacute 667 eacute 444 Ecaron 667 ecaron 444 Ecircumflex 667 ecircumflex 444 Edieresis 667 edieresis 444 Edotaccent 667 edotaccent 444 Egrave 667 egrave 444 eight 500 element 713 ellipsis 1000 Emacron 667 emacron 444 emdash 1000 emptyset 823 endash 500 Eogonek 667 eogonek 444 Epsilon 611 epsilon 439 equal 570 equivalence 549 Eta 722 eta 603 Eth 722 eth 500 Euro 500 exclam 333 exclamdown 333 existential 549 F 611 f 333 fi 556 five 500 fl 556 florin 500 format 600 four 500 fraction 167 G 778 g 500 Gamma 603 gamma 411 Gbreve 778 gbreve 500 Gcaron 600 gcaron 600 Gcommaaccent 778 gcommaaccent 500 germandbls 556 gradient 713 grave 333 graybox 600 greater 570 greaterequal 549 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 778 h 556 heart 753 hungarumlaut 333 hyphen 333 I 389 i 278 Iacute 389 iacute 278 Icircumflex 389 icircumflex 278 Idieresis 389 idieresis 278 Idot 600 Idotaccent 389 Ifraktur 686 Igrave 389 igrave 278 IJ 600 ij 600 Imacron 389 imacron 278 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iogonek 389 iogonek 278 Iota 333 iota 329 J 500 j 333 K 778 k 556 Kappa 722 kappa 549 Kcommaaccent 778 kcommaaccent 556 L 667 l 278 Lacute 667 lacute 278 Lambda 686 lambda 549 largebullet 600 Lcaron 667 lcaron 394 Lcommaaccent 667 lcommaaccent 278 left 600 less 570 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 570 logicalor 603 lozenge 494 Lslash 667 lslash 278 M 944 m 833 macron 333 merge 600 minus 570 minute 247 Mu 889 mu 556 multiply 570 N 722 n 556 Nacute 722 nacute 556 Ncaron 722 ncaron 556 Ncommaaccent 722 ncommaaccent 556 nine 500 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 722 ntilde 556 Nu 722 nu 521 numbersign 500 O 778 o 500 Oacute 778 oacute 500 Ocircumflex 778 ocircumflex 500 Odieresis 778 odieresis 500 OE 1000 oe 722 ogonek 333 Ograve 778 ograve 500 Ohungarumlaut 778 ohungarumlaut 500 Omacron 778 omacron 500 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 300 ordmasculine 330 Oslash 778 oslash 500 Otilde 778 otilde 500 overscore 600 P 611 p 556 paragraph 540 parenleft 333 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 333 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 1000 period 250 periodcentered 250 perpendicular 658 perthousand 1000 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 570 plusminus 570 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 778 q 556 question 500 questiondown 500 quotedbl 555 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 333 quoteright 333 quotesinglbase 333 quotesingle 278 R 722 r 444 Racute 722 racute 444 radical 549 radicalex 500 Rcaron 722 rcaron 444 Rcommaaccent 722 rcommaaccent 444 reflexsubset 713 reflexsuperset 713 registered 747 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 333 S 556 s 389 Sacute 556 sacute 389 Scaron 556 scaron 389 Scedilla 556 scedilla 389 Scommaaccent 556 scommaaccent 389 second 411 section 500 semicolon 333 seven 500 Sigma 592 sigma 603 sigma1 439 similar 549 six 500 slash 278 space 250 spade 753 square 600 sterling 500 stop 600 suchthat 439 summation 600 T 667 t 333 tab 600 Tau 611 tau 439 Tcaron 667 tcaron 416 Tcommaaccent 667 tcommaaccent 333 therefore 863 Theta 741 theta 521 theta1 631 Thorn 611 thorn 556 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 1000 trademarksans 786 trademarkserif 890 two 500 twosuperior 300 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 Uhungarumlaut 722 uhungarumlaut 556 Umacron 722 umacron 556 underscore 500 union 768 universal 713 Uogonek 722 uogonek 556 up 600 Upsilon 690 upsilon 576 Upsilon1 620 Uring 722 uring 556 V 722 v 500 W 1000 w 722 weierstrass 987 X 722 x 500 Xi 645 xi 493 Y 722 y 500 Yacute 722 yacute 500 Ydieresis 722 ydieresis 500 yen 500 Z 667 z 444 Zacute 667 zacute 444 Zcaron 667 zcaron 444 Zdotaccent 667 zdotaccent 444 zero 500 Zeta 611 zeta 494}
set font_widths(Times-BoldItalic) {A 667 a 500 Aacute 667 aacute 500 Abreve 667 abreve 500 Acircumflex 667 acircumflex 500 acute 333 Adieresis 667 adieresis 500 AE 944 ae 722 Agrave 667 agrave 500 aleph 823 Alpha 722 alpha 631 Amacron 667 amacron 500 ampersand 778 angle 768 angleleft 329 angleright 329 Aogonek 667 aogonek 500 apple 790 approxequal 549 Aring 667 aring 500 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 570 asciitilde 570 asterisk 500 asteriskmath 500 at 832 Atilde 667 atilde 500 B 667 b 500 backslash 278 bar 220 Beta 667 beta 549 braceex 494 braceleft 348 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 348 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 333 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 333 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 333 brokenbar 220 bullet 350 C 667 c 444 Cacute 667 cacute 444 caron 333 carriagereturn 658 Ccaron 667 ccaron 444 Ccedilla 667 ccedilla 444 cedilla 333 cent 500 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 333 club 753 colon 333 comma 250 commaaccent 250 congruent 549 copyright 747 copyrightsans 790 copyrightserif 790 currency 500 D 722 d 500 dagger 500 daggerdbl 500 Dcaron 722 dcaron 608 Dcroat 722 dcroat 500 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 333 divide 570 dollar 500 dotaccent 333 dotlessi 278 dotmath 250 down 600 E 667 e 444 Eacute 667 eacute 444 Ecaron 667 ecaron 444 Ecircumflex 667 ecircumflex 444 Edieresis 667 edieresis 444 Edotaccent 667 edotaccent 444 Egrave 667 egrave 444 eight 500 element 713 ellipsis 1000 Emacron 667 emacron 444 emdash 1000 emptyset 823 endash 500 Eogonek 667 eogonek 444 Epsilon 611 epsilon 439 equal 570 equivalence 549 Eta 722 eta 603 Eth 722 eth 500 Euro 500 exclam 389 exclamdown 389 existential 549 F 667 f 333 fi 556 five 500 fl 556 florin 500 format 600 four 500 fraction 167 G 722 g 500 Gamma 603 gamma 411 Gbreve 722 gbreve 500 Gcaron 600 gcaron 600 Gcommaaccent 722 gcommaaccent 500 germandbls 500 gradient 713 grave 333 graybox 600 greater 570 greaterequal 549 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 778 h 556 heart 753 hungarumlaut 333 hyphen 333 I 389 i 278 Iacute 389 iacute 278 Icircumflex 389 icircumflex 278 Idieresis 389 idieresis 278 Idot 600 Idotaccent 389 Ifraktur 686 Igrave 389 igrave 278 IJ 600 ij 600 Imacron 389 imacron 278 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iogonek 389 iogonek 278 Iota 333 iota 329 J 500 j 278 K 667 k 500 Kappa 722 kappa 549 Kcommaaccent 667 kcommaaccent 500 L 611 l 278 Lacute 611 lacute 278 Lambda 686 lambda 549 largebullet 600 Lcaron 611 lcaron 382 Lcommaaccent 611 lcommaaccent 278 left 600 less 570 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 606 logicalor 603 lozenge 494 Lslash 611 lslash 278 M 889 m 778 macron 333 merge 600 minus 606 minute 247 Mu 889 mu 576 multiply 570 N 722 n 556 Nacute 722 nacute 556 Ncaron 722 ncaron 556 Ncommaaccent 722 ncommaaccent 556 nine 500 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 722 ntilde 556 Nu 722 nu 521 numbersign 500 O 722 o 500 Oacute 722 oacute 500 Ocircumflex 722 ocircumflex 500 Odieresis 722 odieresis 500 OE 944 oe 722 ogonek 333 Ograve 722 ograve 500 Ohungarumlaut 722 ohungarumlaut 500 Omacron 722 omacron 500 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 266 ordmasculine 300 Oslash 722 oslash 500 Otilde 722 otilde 500 overscore 600 P 611 p 500 paragraph 500 parenleft 333 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 333 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 833 period 250 periodcentered 250 perpendicular 658 perthousand 1000 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 570 plusminus 570 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 722 q 500 question 500 questiondown 500 quotedbl 555 quotedblbase 500 quotedblleft 500 quotedblright 500 quoteleft 333 quoteright 333 quotesinglbase 333 quotesingle 278 R 667 r 389 Racute 667 racute 389 radical 549 radicalex 500 Rcaron 667 rcaron 389 Rcommaaccent 667 rcommaaccent 389 reflexsubset 713 reflexsuperset 713 registered 747 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 333 S 556 s 389 Sacute 556 sacute 389 Scaron 556 scaron 389 Scedilla 556 scedilla 389 Scommaaccent 556 scommaaccent 389 second 411 section 500 semicolon 333 seven 500 Sigma 592 sigma 603 sigma1 439 similar 549 six 500 slash 278 space 250 spade 753 square 600 sterling 500 stop 600 suchthat 439 summation 600 T 611 t 278 tab 600 Tau 611 tau 439 Tcaron 611 tcaron 366 Tcommaaccent 611 tcommaaccent 278 therefore 863 Theta 741 theta 521 theta1 631 Thorn 611 thorn 500 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 1000 trademarksans 786 trademarkserif 890 two 500 twosuperior 300 U 722 u 556 Uacute 722 uacute 556 Ucircumflex 722 ucircumflex 556 Udieresis 722 udieresis 556 Ugrave 722 ugrave 556 Uhungarumlaut 722 uhungarumlaut 556 Umacron 722 umacron 556 underscore 500 union 768 universal 713 Uogonek 722 uogonek 556 up 600 Upsilon 690 upsilon 576 Upsilon1 620 Uring 722 uring 556 V 667 v 444 W 889 w 667 weierstrass 987 X 667 x 500 Xi 645 xi 493 Y 611 y 444 Yacute 611 yacute 444 Ydieresis 611 ydieresis 444 yen 500 Z 611 z 389 Zacute 611 zacute 389 Zcaron 611 zcaron 389 Zdotaccent 611 zdotaccent 389 zero 500 Zeta 611 zeta 494}
set font_widths(Times-Italic) {A 611 a 500 Aacute 611 aacute 500 Abreve 611 abreve 500 Acircumflex 611 acircumflex 500 acute 333 Adieresis 611 adieresis 500 AE 889 ae 667 Agrave 611 agrave 500 aleph 823 Alpha 722 alpha 631 Amacron 611 amacron 500 ampersand 778 angle 768 angleleft 329 angleright 329 Aogonek 611 aogonek 500 apple 790 approxequal 549 Aring 611 aring 500 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 422 asciitilde 541 asterisk 500 asteriskmath 500 at 920 Atilde 611 atilde 500 B 611 b 500 backslash 278 bar 275 Beta 667 beta 549 braceex 494 braceleft 400 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 400 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 389 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 389 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 333 brokenbar 275 bullet 350 C 667 c 444 Cacute 667 cacute 444 caron 333 carriagereturn 658 Ccaron 667 ccaron 444 Ccedilla 667 ccedilla 444 cedilla 333 cent 500 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 333 club 753 colon 333 comma 250 commaaccent 250 congruent 549 copyright 760 copyrightsans 790 copyrightserif 790 currency 500 D 722 d 500 dagger 500 daggerdbl 500 Dcaron 722 dcaron 544 Dcroat 722 dcroat 500 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 333 divide 675 dollar 500 dotaccent 333 dotlessi 278 dotmath 250 down 600 E 611 e 444 Eacute 611 eacute 444 Ecaron 611 ecaron 444 Ecircumflex 611 ecircumflex 444 Edieresis 611 edieresis 444 Edotaccent 611 edotaccent 444 Egrave 611 egrave 444 eight 500 element 713 ellipsis 889 Emacron 611 emacron 444 emdash 889 emptyset 823 endash 500 Eogonek 611 eogonek 444 Epsilon 611 epsilon 439 equal 675 equivalence 549 Eta 722 eta 603 Eth 722 eth 500 Euro 500 exclam 333 exclamdown 389 existential 549 F 611 f 278 fi 500 five 500 fl 500 florin 500 format 600 four 500 fraction 167 G 722 g 500 Gamma 603 gamma 411 Gbreve 722 gbreve 500 Gcaron 600 gcaron 600 Gcommaaccent 722 gcommaaccent 500 germandbls 500 gradient 713 grave 333 graybox 600 greater 675 greaterequal 549 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 722 h 500 heart 753 hungarumlaut 333 hyphen 333 I 333 i 278 Iacute 333 iacute 278 Icircumflex 333 icircumflex 278 Idieresis 333 idieresis 278 Idot 600 Idotaccent 333 Ifraktur 686 Igrave 333 igrave 278 IJ 600 ij 600 Imacron 333 imacron 278 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iogonek 333 iogonek 278 Iota 333 iota 329 J 444 j 278 K 667 k 444 Kappa 722 kappa 549 Kcommaaccent 667 kcommaaccent 444 L 556 l 278 Lacute 556 lacute 278 Lambda 686 lambda 549 largebullet 600 Lcaron 611 lcaron 300 Lcommaaccent 556 lcommaaccent 278 left 600 less 675 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 675 logicalor 603 lozenge 471 Lslash 556 lslash 278 M 833 m 722 macron 333 merge 600 minus 675 minute 247 Mu 889 mu 500 multiply 675 N 667 n 500 Nacute 667 nacute 500 Ncaron 667 ncaron 500 Ncommaaccent 667 ncommaaccent 500 nine 500 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 667 ntilde 500 Nu 722 nu 521 numbersign 500 O 722 o 500 Oacute 722 oacute 500 Ocircumflex 722 ocircumflex 500 Odieresis 722 odieresis 500 OE 944 oe 667 ogonek 333 Ograve 722 ograve 500 Ohungarumlaut 722 ohungarumlaut 500 Omacron 722 omacron 500 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 276 ordmasculine 310 Oslash 722 oslash 500 Otilde 722 otilde 500 overscore 600 P 611 p 500 paragraph 523 parenleft 333 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 333 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 476 percent 833 period 250 periodcentered 250 perpendicular 658 perthousand 1000 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 675 plusminus 675 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 722 q 500 question 500 questiondown 500 quotedbl 420 quotedblbase 556 quotedblleft 556 quotedblright 556 quoteleft 333 quoteright 333 quotesinglbase 333 quotesingle 214 R 611 r 389 Racute 611 racute 389 radical 453 radicalex 500 Rcaron 611 rcaron 389 Rcommaaccent 611 rcommaaccent 389 reflexsubset 713 reflexsuperset 713 registered 760 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 333 S 500 s 389 Sacute 500 sacute 389 Scaron 500 scaron 389 Scedilla 500 scedilla 389 Scommaaccent 500 scommaaccent 389 second 411 section 500 semicolon 333 seven 500 Sigma 592 sigma 603 sigma1 439 similar 549 six 500 slash 278 space 250 spade 753 square 600 sterling 500 stop 600 suchthat 439 summation 600 T 556 t 278 tab 600 Tau 611 tau 439 Tcaron 556 tcaron 300 Tcommaaccent 556 tcommaaccent 278 therefore 863 Theta 741 theta 521 theta1 631 Thorn 611 thorn 500 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 980 trademarksans 786 trademarkserif 890 two 500 twosuperior 300 U 722 u 500 Uacute 722 uacute 500 Ucircumflex 722 ucircumflex 500 Udieresis 722 udieresis 500 Ugrave 722 ugrave 500 Uhungarumlaut 722 uhungarumlaut 500 Umacron 722 umacron 500 underscore 500 union 768 universal 713 Uogonek 722 uogonek 500 up 600 Upsilon 690 upsilon 576 Upsilon1 620 Uring 722 uring 500 V 611 v 444 W 833 w 667 weierstrass 987 X 611 x 444 Xi 645 xi 493 Y 556 y 444 Yacute 556 yacute 444 Ydieresis 556 ydieresis 444 yen 500 Z 556 z 389 Zacute 556 zacute 389 Zcaron 556 zcaron 389 Zdotaccent 556 zdotaccent 389 zero 500 Zeta 611 zeta 494}
set font_widths(Times-Roman) {A 722 a 444 Aacute 722 aacute 444 Abreve 722 abreve 444 Acircumflex 722 acircumflex 444 acute 333 Adieresis 722 adieresis 444 AE 889 ae 667 Agrave 722 agrave 444 aleph 823 Alpha 722 alpha 631 Amacron 722 amacron 444 ampersand 778 angle 768 angleleft 329 angleright 329 Aogonek 722 aogonek 444 apple 790 approxequal 549 Aring 722 aring 444 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 469 asciitilde 541 asterisk 500 asteriskmath 500 at 921 Atilde 722 atilde 444 B 667 b 500 backslash 278 bar 200 Beta 667 beta 549 braceex 494 braceleft 480 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 480 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 333 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 333 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 333 brokenbar 200 bullet 350 C 667 c 444 Cacute 667 cacute 444 caron 333 carriagereturn 658 Ccaron 667 ccaron 444 Ccedilla 667 ccedilla 444 cedilla 333 cent 500 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 333 club 753 colon 278 comma 250 commaaccent 250 congruent 549 copyright 760 copyrightsans 790 copyrightserif 790 currency 500 D 722 d 500 dagger 500 daggerdbl 500 Dcaron 722 dcaron 588 Dcroat 722 dcroat 500 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 333 divide 564 dollar 500 dotaccent 333 dotlessi 278 dotmath 250 down 600 E 611 e 444 Eacute 611 eacute 444 Ecaron 611 ecaron 444 Ecircumflex 611 ecircumflex 444 Edieresis 611 edieresis 444 Edotaccent 611 edotaccent 444 Egrave 611 egrave 444 eight 500 element 713 ellipsis 1000 Emacron 611 emacron 444 emdash 1000 emptyset 823 endash 500 Eogonek 611 eogonek 444 Epsilon 611 epsilon 439 equal 564 equivalence 549 Eta 722 eta 603 Eth 722 eth 500 Euro 500 exclam 333 exclamdown 333 existential 549 F 556 f 333 fi 556 five 500 fl 556 florin 500 format 600 four 500 fraction 167 G 722 g 500 Gamma 603 gamma 411 Gbreve 722 gbreve 500 Gcaron 600 gcaron 600 Gcommaaccent 722 gcommaaccent 500 germandbls 500 gradient 713 grave 333 graybox 600 greater 564 greaterequal 549 guillemotleft 500 guillemotright 500 guilsinglleft 333 guilsinglright 333 H 722 h 500 heart 753 hungarumlaut 333 hyphen 333 I 333 i 278 Iacute 333 iacute 278 Icircumflex 333 icircumflex 278 Idieresis 333 idieresis 278 Idot 600 Idotaccent 333 Ifraktur 686 Igrave 333 igrave 278 IJ 600 ij 600 Imacron 333 imacron 278 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iogonek 333 iogonek 278 Iota 333 iota 329 J 389 j 278 K 722 k 500 Kappa 722 kappa 549 Kcommaaccent 722 kcommaaccent 500 L 611 l 278 Lacute 611 lacute 278 Lambda 686 lambda 549 largebullet 600 Lcaron 611 lcaron 344 Lcommaaccent 611 lcommaaccent 278 left 600 less 564 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 564 logicalor 603 lozenge 471 Lslash 611 lslash 278 M 889 m 778 macron 333 merge 600 minus 564 minute 247 Mu 889 mu 500 multiply 564 N 722 n 500 Nacute 722 nacute 500 Ncaron 722 ncaron 500 Ncommaaccent 722 ncommaaccent 500 nine 500 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 722 ntilde 500 Nu 722 nu 521 numbersign 500 O 722 o 500 Oacute 722 oacute 500 Ocircumflex 722 ocircumflex 500 Odieresis 722 odieresis 500 OE 889 oe 722 ogonek 333 Ograve 722 ograve 500 Ohungarumlaut 722 ohungarumlaut 500 Omacron 722 omacron 500 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 500 onehalf 750 onequarter 750 onesuperior 300 ordfeminine 276 ordmasculine 310 Oslash 722 oslash 500 Otilde 722 otilde 500 overscore 600 P 556 p 500 paragraph 453 parenleft 333 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 333 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 476 percent 833 period 250 periodcentered 250 perpendicular 658 perthousand 1000 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 564 plusminus 564 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 722 q 500 question 444 questiondown 444 quotedbl 408 quotedblbase 444 quotedblleft 444 quotedblright 444 quoteleft 333 quoteright 333 quotesinglbase 333 quotesingle 180 R 667 r 333 Racute 667 racute 333 radical 453 radicalex 500 Rcaron 667 rcaron 333 Rcommaaccent 667 rcommaaccent 333 reflexsubset 713 reflexsuperset 713 registered 760 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 333 S 556 s 389 Sacute 556 sacute 389 Scaron 556 scaron 389 Scedilla 556 scedilla 389 Scommaaccent 556 scommaaccent 389 second 411 section 500 semicolon 278 seven 500 Sigma 592 sigma 603 sigma1 439 similar 549 six 500 slash 278 space 250 spade 753 square 600 sterling 500 stop 600 suchthat 439 summation 600 T 611 t 278 tab 600 Tau 611 tau 439 Tcaron 611 tcaron 326 Tcommaaccent 611 tcommaaccent 278 therefore 863 Theta 741 theta 521 theta1 631 Thorn 556 thorn 500 three 500 threequarters 750 threesuperior 300 tilde 333 trademark 980 trademarksans 786 trademarkserif 890 two 500 twosuperior 300 U 722 u 500 Uacute 722 uacute 500 Ucircumflex 722 ucircumflex 500 Udieresis 722 udieresis 500 Ugrave 722 ugrave 500 Uhungarumlaut 722 uhungarumlaut 500 Umacron 722 umacron 500 underscore 500 union 768 universal 713 Uogonek 722 uogonek 500 up 600 Upsilon 690 upsilon 576 Upsilon1 620 Uring 722 uring 500 V 722 v 500 W 944 w 722 weierstrass 987 X 722 x 500 Xi 645 xi 493 Y 722 y 500 Yacute 722 yacute 500 Ydieresis 722 ydieresis 500 yen 500 Z 611 z 444 Zacute 611 zacute 444 Zcaron 611 zcaron 444 Zdotaccent 611 zdotaccent 444 zero 500 Zeta 611 zeta 494}
set font_widths(Utopia-Bold) {A 644 a 544 Aacute 644 aacute 544 Acircumflex 644 acircumflex 544 acute 430 Adieresis 644 adieresis 544 AE 879 ae 806 Agrave 644 agrave 544 aleph 823 Alpha 722 alpha 631 ampersand 748 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 644 aring 544 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 600 asciitilde 600 asterisk 442 asteriskmath 500 at 833 Atilde 644 atilde 544 B 683 b 605 backslash 379 bar 284 Beta 667 beta 549 braceex 494 braceleft 365 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 365 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 335 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 335 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 430 brokenbar 284 bullet 455 C 689 c 494 caron 430 carriagereturn 658 Ccedilla 689 ccedilla 494 cedilla 430 cent 560 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 430 club 753 colon 280 comma 280 congruent 549 copyright 800 copyrightsans 790 copyrightserif 790 currency 560 D 777 d 605 dagger 510 daggerdbl 486 dectab 600 degree 396 Delta 612 delta 494 diamond 753 dieresis 430 divide 600 dollar 560 dotaccent 430 dotlessi 316 dotmath 250 down 600 E 629 e 519 Eacute 629 eacute 519 Ecircumflex 629 ecircumflex 519 Edieresis 629 edieresis 519 Egrave 629 egrave 519 eight 560 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 600 equivalence 549 Eta 722 eta 603 Eth 783 eth 585 exclam 278 exclamdown 278 existential 549 F 593 f 342 fi 639 five 560 fl 639 florin 560 format 600 four 560 fraction 100 G 726 g 533 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 662 gradient 713 grave 430 graybox 600 greater 600 greaterequal 549 guillemotleft 487 guillemotright 487 guilsinglleft 287 guilsinglright 287 H 807 h 631 heart 753 hungarumlaut 430 hyphen 392 I 384 i 316 Iacute 384 iacute 316 Icircumflex 384 icircumflex 316 Idieresis 384 idieresis 316 Idot 600 Ifraktur 686 Igrave 384 igrave 316 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 386 j 316 K 707 k 582 Kappa 722 kappa 549 L 585 l 309 Lambda 686 lambda 549 largebullet 600 left 600 less 600 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 600 logicalor 603 lozenge 494 Lslash 591 lslash 321 M 918 m 948 macron 430 merge 600 minus 600 minute 247 Mu 889 mu 629 multiply 600 N 739 n 638 nine 560 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 739 ntilde 638 Nu 722 nu 521 numbersign 560 O 768 o 585 Oacute 768 oacute 585 Ocircumflex 768 ocircumflex 585 Odieresis 768 odieresis 585 OE 1049 oe 866 ogonek 430 Ograve 768 ograve 585 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 560 onehalf 900 onequarter 900 onesuperior 402 ordfeminine 405 ordmasculine 427 Oslash 768 oslash 585 Otilde 768 otilde 585 overscore 600 P 650 p 615 paragraph 552 parenleft 365 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 365 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 887 period 280 periodcentered 280 perpendicular 658 perthousand 1289 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 600 plusminus 600 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 768 q 597 question 456 questiondown 456 quotedbl 473 quotedblbase 473 quotedblleft 473 quotedblright 473 quoteleft 252 quoteright 252 quotesinglbase 252 quotesingle 252 R 684 r 440 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 800 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 430 S 561 s 446 Scaron 561 scaron 446 Scedilla 600 scedilla 600 second 411 section 566 semicolon 280 seven 560 Sigma 592 sigma 603 sigma1 439 similar 549 six 560 slash 378 space 210 spade 753 square 600 sterling 560 stop 600 suchthat 439 summation 713 T 624 t 370 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 640 thorn 609 three 560 threequarters 900 threesuperior 402 tilde 430 trademark 1090 trademarksans 786 trademarkserif 890 two 560 twosuperior 402 U 786 u 629 Uacute 786 uacute 629 Ucircumflex 786 ucircumflex 629 Udieresis 786 udieresis 629 Ugrave 786 ugrave 629 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 645 v 520 W 933 w 774 weierstrass 987 X 634 x 522 Xi 645 xi 493 Y 617 y 524 Yacute 617 yacute 524 Ydieresis 617 ydieresis 524 yen 560 Z 614 z 483 Zcaron 614 zcaron 483 zero 560 Zeta 611 zeta 494}
set font_widths(Utopia-BoldItalic) {A 634 a 596 Aacute 634 aacute 596 Acircumflex 634 acircumflex 596 acute 400 Adieresis 634 adieresis 596 AE 890 ae 789 Agrave 634 agrave 596 aleph 823 Alpha 722 alpha 631 ampersand 752 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 634 aring 596 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 600 asciitilde 600 asterisk 500 asteriskmath 500 at 828 Atilde 634 atilde 596 B 680 b 586 backslash 460 bar 265 Beta 667 beta 549 braceex 494 braceleft 340 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 340 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 350 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 350 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 400 brokenbar 265 bullet 465 C 672 c 456 caron 400 carriagereturn 658 Ccedilla 672 ccedilla 456 cedilla 400 cent 560 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 400 club 753 colon 280 comma 280 congruent 549 copyright 824 copyrightsans 790 copyrightserif 790 currency 560 D 774 d 609 dagger 514 daggerdbl 490 dectab 600 degree 375 Delta 612 delta 494 diamond 753 dieresis 400 divide 600 dollar 560 dotaccent 402 dotlessi 339 dotmath 250 down 600 E 622 e 476 Eacute 622 eacute 476 Ecircumflex 622 ecircumflex 476 Edieresis 622 edieresis 476 Egrave 622 egrave 476 eight 560 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 600 equivalence 549 Eta 722 eta 603 Eth 780 eth 562 exclam 285 exclamdown 285 existential 549 F 585 f 348 fi 651 five 560 fl 652 florin 560 format 600 four 560 fraction 100 G 726 g 522 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 628 gradient 713 grave 400 graybox 600 greater 600 greaterequal 549 guillemotleft 560 guillemotright 560 guilsinglleft 360 guilsinglright 360 H 800 h 629 heart 753 hungarumlaut 400 hyphen 392 I 386 i 339 Iacute 386 iacute 339 Icircumflex 386 icircumflex 339 Idieresis 386 idieresis 339 Idot 600 Ifraktur 686 Igrave 386 igrave 339 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 388 j 333 K 688 k 570 Kappa 722 kappa 549 L 586 l 327 Lambda 686 lambda 549 largebullet 600 left 600 less 600 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 600 logicalor 603 lozenge 494 Lslash 592 lslash 339 M 921 m 914 macron 400 merge 600 minus 600 minute 247 Mu 889 mu 634 multiply 600 N 741 n 635 nine 560 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 741 ntilde 635 Nu 722 nu 521 numbersign 560 O 761 o 562 Oacute 761 oacute 562 Ocircumflex 761 ocircumflex 562 Odieresis 761 odieresis 562 OE 1016 oe 811 ogonek 350 Ograve 761 ograve 562 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 560 onehalf 940 onequarter 940 onesuperior 402 ordfeminine 444 ordmasculine 412 Oslash 761 oslash 562 Otilde 761 otilde 562 overscore 600 P 660 p 606 paragraph 580 parenleft 350 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 350 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 896 period 280 periodcentered 280 perpendicular 658 perthousand 1297 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 600 plusminus 600 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 761 q 584 question 454 questiondown 454 quotedbl 455 quotedblbase 455 quotedblleft 455 quotedblright 455 quoteleft 246 quoteright 246 quotesinglbase 246 quotesingle 246 R 681 r 440 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 824 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 400 S 551 s 417 Scaron 551 scaron 417 Scedilla 600 scedilla 600 second 411 section 568 semicolon 280 seven 560 Sigma 592 sigma 603 sigma1 439 similar 549 six 560 slash 260 space 210 spade 753 square 600 sterling 560 stop 600 suchthat 439 summation 713 T 616 t 359 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 629 thorn 600 three 560 threequarters 940 threesuperior 402 tilde 400 trademark 1100 trademarksans 786 trademarkserif 890 two 560 twosuperior 402 U 776 u 634 Uacute 776 uacute 634 Ucircumflex 776 ucircumflex 634 Udieresis 776 udieresis 634 Ugrave 776 ugrave 634 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 630 v 518 W 920 w 795 weierstrass 987 X 630 x 516 Xi 645 xi 493 Y 622 y 489 Yacute 622 yacute 489 Ydieresis 622 ydieresis 489 yen 560 Z 618 z 466 Zcaron 618 zcaron 466 zero 560 Zeta 611 zeta 494}
set font_widths(Utopia-Italic) {A 624 a 561 Aacute 624 aacute 561 Acircumflex 624 acircumflex 561 acute 400 Adieresis 624 adieresis 561 AE 880 ae 779 Agrave 624 agrave 561 aleph 823 Alpha 722 alpha 631 ampersand 725 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 624 aring 561 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 570 asciitilde 570 asterisk 412 asteriskmath 500 at 794 Atilde 624 atilde 561 B 632 b 559 backslash 390 bar 270 Beta 667 beta 549 braceex 494 braceleft 340 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 340 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 330 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 330 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 400 brokenbar 270 bullet 500 C 661 c 441 caron 400 carriagereturn 658 Ccedilla 661 ccedilla 441 cedilla 400 cent 530 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 400 club 753 colon 265 comma 265 congruent 549 copyright 836 copyrightsans 790 copyrightserif 790 currency 530 D 763 d 587 dagger 500 daggerdbl 490 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 400 divide 570 dollar 530 dotaccent 402 dotlessi 317 dotmath 250 down 600 E 596 e 453 Eacute 596 eacute 453 Ecircumflex 596 ecircumflex 453 Edieresis 596 edieresis 453 Egrave 596 egrave 453 eight 530 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 570 equivalence 549 Eta 722 eta 603 Eth 770 eth 537 exclam 240 exclamdown 240 existential 549 F 571 f 315 fi 607 five 530 fl 603 florin 530 format 600 four 530 fraction 100 G 709 g 499 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 577 gradient 713 grave 400 graybox 600 greater 570 greaterequal 549 guillemotleft 462 guillemotright 462 guilsinglleft 277 guilsinglright 277 H 775 h 607 heart 753 hungarumlaut 400 hyphen 392 I 345 i 317 Iacute 345 iacute 317 Icircumflex 345 icircumflex 317 Idieresis 345 idieresis 317 Idot 600 Ifraktur 686 Igrave 345 igrave 317 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 352 j 309 K 650 k 545 Kappa 722 kappa 549 L 565 l 306 Lambda 686 lambda 549 largebullet 600 left 600 less 570 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 570 logicalor 603 lozenge 494 Lslash 571 lslash 318 M 920 m 912 macron 400 merge 600 minus 570 minute 247 Mu 889 mu 618 multiply 570 N 763 n 618 nine 530 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 763 ntilde 618 Nu 722 nu 521 numbersign 530 O 753 o 537 Oacute 753 oacute 537 Ocircumflex 753 ocircumflex 537 Odieresis 753 odieresis 537 OE 1020 oe 806 ogonek 350 Ograve 753 ograve 537 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 530 onehalf 890 onequarter 890 onesuperior 370 ordfeminine 425 ordmasculine 389 Oslash 753 oslash 537 Otilde 753 otilde 537 overscore 600 P 614 p 590 paragraph 560 parenleft 350 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 350 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 826 period 265 periodcentered 265 perpendicular 658 perthousand 1200 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 570 plusminus 570 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 753 q 559 question 425 questiondown 425 quotedbl 402 quotedblbase 402 quotedblleft 402 quotedblright 402 quoteleft 216 quoteright 216 quotesinglbase 216 quotesingle 216 R 640 r 402 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 836 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 400 S 533 s 389 Scaron 533 scaron 389 Scedilla 600 scedilla 600 second 411 section 530 semicolon 265 seven 530 Sigma 592 sigma 603 sigma1 439 similar 549 six 530 slash 270 space 225 spade 753 square 600 sterling 530 stop 600 suchthat 439 summation 713 T 606 t 341 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 604 thorn 584 three 530 threequarters 890 threesuperior 370 tilde 400 trademark 1100 trademarksans 786 trademarkserif 890 two 530 twosuperior 370 U 794 u 618 Uacute 794 uacute 618 Ucircumflex 794 ucircumflex 618 Udieresis 794 udieresis 618 Ugrave 794 ugrave 618 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 637 v 510 W 946 w 785 weierstrass 987 X 632 x 516 Xi 645 xi 493 Y 591 y 468 Yacute 591 yacute 468 Ydieresis 591 ydieresis 468 yen 530 Z 622 z 468 Zcaron 622 zcaron 468 zero 530 Zeta 611 zeta 494}
set font_widths(Utopia-Regular) {A 635 a 523 Aacute 635 aacute 523 Acircumflex 635 acircumflex 523 acute 400 Adieresis 635 adieresis 523 AE 876 ae 797 Agrave 635 agrave 523 aleph 823 Alpha 722 alpha 631 ampersand 706 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 627 aring 523 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 570 asciitilde 570 asterisk 412 asteriskmath 500 at 793 Atilde 635 atilde 523 B 646 b 598 backslash 460 bar 228 Beta 667 beta 549 braceex 494 braceleft 340 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 340 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 330 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 330 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 400 brokenbar 228 bullet 409 C 684 c 496 caron 400 carriagereturn 658 Ccedilla 680 ccedilla 496 cedilla 400 cent 530 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 400 club 753 colon 265 comma 265 congruent 549 copyright 818 copyrightsans 790 copyrightserif 790 currency 530 D 779 d 598 dagger 504 daggerdbl 488 dectab 600 degree 350 Delta 612 delta 494 diamond 753 dieresis 400 divide 570 dollar 530 dotaccent 400 dotlessi 291 dotmath 250 down 600 E 606 e 514 Eacute 606 eacute 514 Ecircumflex 606 ecircumflex 514 Edieresis 606 edieresis 514 Egrave 606 egrave 514 eight 530 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 570 equivalence 549 Eta 722 eta 603 Eth 785 eth 577 exclam 242 exclamdown 242 existential 549 F 580 f 319 fi 610 five 530 fl 610 florin 530 format 600 four 530 fraction 150 G 734 g 520 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 601 gradient 713 grave 400 graybox 600 greater 570 greaterequal 549 guillemotleft 442 guillemotright 442 guilsinglleft 257 guilsinglright 257 H 798 h 607 heart 753 hungarumlaut 400 hyphen 392 I 349 i 291 Iacute 349 iacute 291 Icircumflex 349 icircumflex 291 Idieresis 349 idieresis 291 Idot 600 Ifraktur 686 Igrave 349 igrave 291 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 350 j 280 K 658 k 524 Kappa 722 kappa 549 L 568 l 279 Lambda 686 lambda 549 largebullet 600 left 600 less 570 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 570 logicalor 603 lozenge 494 Lslash 574 lslash 294 M 944 m 923 macron 400 merge 600 minus 570 minute 247 Mu 889 mu 606 multiply 570 N 780 n 619 nine 530 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 780 ntilde 619 Nu 722 nu 521 numbersign 530 O 762 o 577 Oacute 762 oacute 577 Ocircumflex 762 ocircumflex 577 Odieresis 762 odieresis 577 OE 1025 oe 882 ogonek 400 Ograve 762 ograve 577 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 530 onehalf 860 onequarter 860 onesuperior 380 ordfeminine 390 ordmasculine 398 Oslash 762 oslash 577 Otilde 762 otilde 577 overscore 600 P 600 p 608 paragraph 555 parenleft 350 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 350 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 838 period 265 periodcentered 265 perpendicular 658 perthousand 1208 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 570 plusminus 570 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 762 q 591 question 389 questiondown 389 quotedbl 458 quotedblbase 458 quotedblleft 458 quotedblright 458 quoteleft 278 quoteright 278 quotesinglbase 278 quotesingle 278 R 644 r 389 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 818 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 400 S 541 s 436 Scaron 541 scaron 436 Scedilla 600 scedilla 600 second 411 section 554 semicolon 265 seven 530 Sigma 592 sigma 603 sigma1 439 similar 549 six 530 slash 460 space 225 spade 753 square 600 sterling 530 stop 600 suchthat 439 summation 713 T 621 t 344 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 593 thorn 606 three 530 threequarters 860 threesuperior 380 tilde 400 trademark 1100 trademarksans 786 trademarkserif 890 two 530 twosuperior 380 U 791 u 606 Uacute 791 uacute 606 Ucircumflex 791 ucircumflex 606 Udieresis 791 udieresis 606 Ugrave 791 ugrave 606 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 634 v 504 W 940 w 768 weierstrass 987 X 624 x 486 Xi 645 xi 493 Y 588 y 506 Yacute 588 yacute 506 Ydieresis 588 ydieresis 506 yen 530 Z 610 z 480 Zcaron 610 zcaron 480 zero 530 Zeta 611 zeta 494}
set font_widths(ZapfChancery-MediumItalic) {A 620 a 420 Aacute 620 aacute 420 Acircumflex 620 acircumflex 420 acute 300 Adieresis 620 adieresis 420 AE 740 ae 540 Agrave 620 agrave 420 aleph 823 Alpha 722 alpha 631 ampersand 780 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 620 aring 420 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 520 asciitilde 520 asterisk 420 asteriskmath 500 at 700 Atilde 620 atilde 420 B 600 b 420 backslash 480 bar 520 Beta 667 beta 549 braceex 494 braceleft 240 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 240 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 240 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 320 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 440 brokenbar 520 bullet 600 C 520 c 340 caron 340 carriagereturn 658 Ccedilla 520 ccedilla 340 cedilla 300 cent 440 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 340 club 753 colon 260 comma 220 congruent 549 copyright 740 copyrightsans 790 copyrightserif 790 currency 440 D 700 d 440 dagger 460 daggerdbl 480 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 360 divide 520 dollar 440 dotaccent 220 dotlessi 240 dotmath 250 down 600 E 620 e 340 Eacute 620 eacute 340 Ecircumflex 620 ecircumflex 340 Edieresis 620 edieresis 340 Egrave 620 egrave 340 eight 440 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 520 equivalence 549 Eta 722 eta 603 Eth 700 eth 400 exclam 280 exclamdown 280 existential 549 F 580 f 320 fi 520 five 440 fl 520 florin 440 format 600 four 440 fraction 60 G 620 g 400 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 420 gradient 713 grave 220 graybox 600 greater 520 greaterequal 549 guillemotleft 340 guillemotright 380 guilsinglleft 240 guilsinglright 260 H 680 h 440 heart 753 hungarumlaut 400 hyphen 280 I 380 i 240 Iacute 380 iacute 240 Icircumflex 380 icircumflex 240 Idieresis 380 idieresis 240 Idot 600 Ifraktur 686 Igrave 380 igrave 240 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 400 j 220 K 660 k 440 Kappa 722 kappa 549 L 580 l 240 Lambda 686 lambda 549 largebullet 600 left 600 less 520 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 520 logicalor 603 lozenge 494 Lslash 580 lslash 300 M 840 m 620 macron 440 merge 600 minus 520 minute 247 Mu 889 mu 460 multiply 520 N 700 n 460 nine 440 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 700 ntilde 460 Nu 722 nu 521 numbersign 440 O 600 o 400 Oacute 600 oacute 400 Ocircumflex 600 ocircumflex 400 Odieresis 600 odieresis 400 OE 820 oe 560 ogonek 280 Ograve 600 ograve 400 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 440 onehalf 660 onequarter 660 onesuperior 264 ordfeminine 260 ordmasculine 260 Oslash 660 oslash 440 Otilde 600 otilde 400 overscore 600 P 540 p 440 paragraph 500 parenleft 260 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 220 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 680 period 220 periodcentered 220 perpendicular 658 perthousand 960 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 520 plusminus 520 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 600 q 400 question 380 questiondown 400 quotedbl 220 quotedblbase 280 quotedblleft 340 quotedblright 360 quoteleft 240 quoteright 240 quotesinglbase 180 quotesingle 160 R 600 r 300 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 740 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 300 S 460 s 320 Scaron 460 scaron 320 Scedilla 600 scedilla 600 second 411 section 420 semicolon 240 seven 440 Sigma 592 sigma 603 sigma1 439 similar 549 six 440 slash 340 space 220 spade 753 square 600 sterling 440 stop 600 suchthat 439 summation 713 T 500 t 320 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 540 thorn 440 three 440 threequarters 660 threesuperior 264 tilde 440 trademark 1000 trademarksans 786 trademarkserif 890 two 440 twosuperior 264 U 740 u 460 Uacute 740 uacute 460 Ucircumflex 740 ucircumflex 460 Udieresis 740 udieresis 460 Ugrave 740 ugrave 460 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 640 v 440 W 880 w 680 weierstrass 987 X 560 x 420 Xi 645 xi 493 Y 560 y 400 Yacute 560 yacute 400 Ydieresis 560 ydieresis 400 yen 440 Z 620 z 440 Zcaron 620 zcaron 440 zero 440 Zeta 611 zeta 494}
set font_widths(ZapfDingbats) {A 620 a 420 a1 974 a2 961 a3 980 a4 719 a5 789 a6 494 a7 552 a8 537 a9 577 a10 692 a11 960 a12 939 a13 549 a14 855 a15 911 a16 933 a17 945 a18 974 a19 755 a20 846 a21 762 a22 761 a23 571 a24 677 a25 763 a26 760 a27 759 a28 754 a29 786 a30 788 a31 788 a32 790 a33 793 a34 794 a35 816 a36 823 a37 789 a38 841 a39 823 a40 833 a41 816 a42 831 a43 923 a44 744 a45 723 a46 749 a47 790 a48 792 a49 695 a50 776 a51 768 a52 792 a53 759 a54 707 a55 708 a56 682 a57 701 a58 826 a59 815 a60 789 a61 789 a62 707 a63 687 a64 696 a65 689 a66 786 a67 787 a68 713 a69 791 a70 785 a71 791 a72 873 a73 761 a74 762 a75 759 a76 892 a77 892 a78 788 a79 784 a81 438 a82 138 a83 277 a84 415 a85 509 a86 410 a87 234 a88 234 a89 390 a90 390 a91 276 a92 276 a93 317 a94 317 a95 334 a96 334 a97 392 a98 392 a99 668 a100 668 a101 732 a102 544 a103 544 a104 910 a105 911 a106 667 a107 760 a108 760 a109 626 a110 694 a111 595 a112 776 a117 690 a118 791 a119 790 a120 788 a121 788 a122 788 a123 788 a124 788 a125 788 a126 788 a127 788 a128 788 a129 788 a130 788 a131 788 a132 788 a133 788 a134 788 a135 788 a136 788 a137 788 a138 788 a139 788 a140 788 a141 788 a142 788 a143 788 a144 788 a145 788 a146 788 a147 788 a148 788 a149 788 a150 788 a151 788 a152 788 a153 788 a154 788 a155 788 a156 788 a157 788 a158 788 a159 788 a160 894 a161 838 a162 924 a163 1016 a164 458 a165 924 a166 918 a167 927 a168 928 a169 928 a170 834 a171 873 a172 828 a173 924 a174 917 a175 930 a176 931 a177 463 a178 883 a179 836 a180 867 a181 696 a182 874 a183 760 a184 946 a185 865 a186 967 a187 831 a188 873 a189 927 a190 970 a191 918 a192 748 a193 836 a194 771 a195 888 a196 748 a197 771 a198 888 a199 867 a200 696 a201 874 a202 974 a203 762 a204 759 a205 509 a206 410 Aacute 620 aacute 420 Acircumflex 620 acircumflex 420 acute 300 Adieresis 620 adieresis 420 AE 740 ae 540 Agrave 620 agrave 420 aleph 823 Alpha 722 alpha 631 ampersand 780 angle 768 angleleft 329 angleright 329 apple 790 approxequal 549 Aring 620 aring 420 arrowboth 1042 arrowdblboth 1042 arrowdbldown 603 arrowdblleft 987 arrowdblright 987 arrowdblup 603 arrowdown 603 arrowhorizex 1000 arrowleft 987 arrowright 987 arrowup 603 arrowvertex 603 asciicircum 520 asciitilde 520 asterisk 420 asteriskmath 500 at 700 Atilde 620 atilde 420 B 600 b 420 backslash 480 bar 520 Beta 667 beta 549 braceex 494 braceleft 240 braceleftbt 494 braceleftmid 494 bracelefttp 494 braceright 240 bracerightbt 494 bracerightmid 494 bracerighttp 494 bracketleft 240 bracketleftbt 384 bracketleftex 384 bracketlefttp 384 bracketright 320 bracketrightbt 384 bracketrightex 384 bracketrighttp 384 breve 440 brokenbar 520 bullet 600 C 520 c 340 caron 340 carriagereturn 658 Ccedilla 520 ccedilla 340 cedilla 300 cent 440 center 600 Chi 722 chi 549 circlemultiply 768 circleplus 768 circumflex 340 club 753 colon 260 comma 220 congruent 549 copyright 740 copyrightsans 790 copyrightserif 790 currency 440 D 700 d 440 dagger 460 daggerdbl 480 dectab 600 degree 400 Delta 612 delta 494 diamond 753 dieresis 360 divide 520 dollar 440 dotaccent 220 dotlessi 240 dotmath 250 down 600 E 620 e 340 Eacute 620 eacute 340 Ecircumflex 620 ecircumflex 340 Edieresis 620 edieresis 340 Egrave 620 egrave 340 eight 440 element 713 ellipsis 1000 emdash 1000 emptyset 823 endash 500 Epsilon 611 epsilon 439 equal 520 equivalence 549 Eta 722 eta 603 Eth 700 eth 400 exclam 280 exclamdown 280 existential 549 F 580 f 320 fi 520 five 440 fl 520 florin 440 format 600 four 440 fraction 60 G 620 g 400 Gamma 603 gamma 411 Gcaron 600 gcaron 600 germandbls 420 gradient 713 grave 220 graybox 600 greater 520 greaterequal 549 guillemotleft 340 guillemotright 380 guilsinglleft 240 guilsinglright 260 H 680 h 440 heart 753 hungarumlaut 400 hyphen 280 I 380 i 240 Iacute 380 iacute 240 Icircumflex 380 icircumflex 240 Idieresis 380 idieresis 240 Idot 600 Ifraktur 686 Igrave 380 igrave 240 IJ 600 ij 600 indent 600 infinity 713 integral 274 integralbt 686 integralex 686 integraltp 686 intersection 768 Iota 333 iota 329 J 400 j 220 K 660 k 440 Kappa 722 kappa 549 L 580 l 240 Lambda 686 lambda 549 largebullet 600 left 600 less 520 lessequal 549 lira 600 LL 600 ll 600 logicaland 603 logicalnot 520 logicalor 603 lozenge 494 Lslash 580 lslash 300 M 840 m 620 macron 440 merge 600 minus 520 minute 247 Mu 889 mu 460 multiply 520 N 700 n 460 nine 440 notegraphic 600 notelement 713 notequal 549 notsubset 713 Ntilde 700 ntilde 460 Nu 722 nu 521 numbersign 440 O 600 o 400 Oacute 600 oacute 400 Ocircumflex 600 ocircumflex 400 Odieresis 600 odieresis 400 OE 820 oe 560 ogonek 280 Ograve 600 ograve 400 Omega 768 omega 686 omega1 713 Omicron 722 omicron 549 one 440 onehalf 660 onequarter 660 onesuperior 264 ordfeminine 260 ordmasculine 260 Oslash 660 oslash 440 Otilde 600 otilde 400 overscore 600 P 540 p 440 paragraph 500 parenleft 260 parenleftbt 384 parenleftex 384 parenlefttp 384 parenright 220 parenrightbt 384 parenrightex 384 parenrighttp 384 partialdiff 494 percent 680 period 220 periodcentered 220 perpendicular 658 perthousand 960 Phi 763 phi 521 phi1 603 Pi 768 pi 549 plus 520 plusminus 520 prescription 600 product 823 propersubset 713 propersuperset 713 proportional 713 Psi 795 psi 686 Q 600 q 400 question 380 questiondown 400 quotedbl 220 quotedblbase 280 quotedblleft 340 quotedblright 360 quoteleft 240 quoteright 240 quotesinglbase 180 quotesingle 160 R 600 r 300 radical 549 radicalex 500 reflexsubset 713 reflexsuperset 713 registered 740 registersans 790 registerserif 790 return 600 Rfraktur 795 Rho 556 rho 549 ring 300 S 460 s 320 Scaron 460 scaron 320 Scedilla 600 scedilla 600 second 411 section 420 semicolon 240 seven 440 Sigma 592 sigma 603 sigma1 439 similar 549 six 440 slash 340 space 278 spade 753 square 600 sterling 440 stop 600 suchthat 439 summation 713 T 500 t 320 tab 600 Tau 611 tau 439 therefore 863 Theta 741 theta 521 theta1 631 Thorn 540 thorn 440 three 440 threequarters 660 threesuperior 264 tilde 440 trademark 1000 trademarksans 786 trademarkserif 890 two 440 twosuperior 264 U 740 u 460 Uacute 740 uacute 460 Ucircumflex 740 ucircumflex 460 Udieresis 740 udieresis 460 Ugrave 740 ugrave 460 underscore 500 union 768 universal 713 up 600 Upsilon 690 upsilon 576 Upsilon1 620 V 640 v 440 W 880 w 680 weierstrass 987 X 560 x 420 Xi 645 xi 493 Y 560 y 400 Yacute 560 yacute 400 Ydieresis 560 ydieresis 400 yen 440 Z 620 z 440 Zcaron 620 zcaron 440 zero 440 Zeta 611 zeta 494}
set font_metrics(AvantGarde-Book) {ascend 740 bbox {-113 -222 1148 955} descend -192 fixed 0}
set font_metrics(AvantGarde-BookOblique) {ascend 740 bbox {-113 -222 1279 955} descend -192 fixed 0}
set font_metrics(AvantGarde-Demi) {ascend 740 bbox {-123 -251 1222 1021} descend -185 fixed 0}
set font_metrics(AvantGarde-DemiOblique) {ascend 740 bbox {-123 -251 1256 1021} descend -185 fixed 0}
set font_metrics(Bookman-Demi) {ascend 725 bbox {-194 -243 1346 934} descend -212 fixed 0}
set font_metrics(Bookman-DemiItalic) {ascend 732 bbox {-231 -220 1333 941} descend -213 fixed 0}
set font_metrics(Bookman-Light) {ascend 717 bbox {-188 -251 1266 928} descend -228 fixed 0}
set font_metrics(Bookman-LightItalic) {ascend 717 bbox {-228 -222 1269 893} descend -212 fixed 0}
set font_metrics(Courier) {ascend 629 bbox {-23 -250 715 805 } descend -157 fixed 1}
set font_metrics(Courier-Bold) {ascend 629 bbox {-113 -250 749 801 } descend -157 fixed 1}
set font_metrics(Courier-BoldOblique) {ascend 629 bbox {-57 -250 869 801 } descend -157 fixed 1}
set font_metrics(Courier-Oblique) {ascend 629 bbox {-27 -250 849 805 } descend -157 fixed 1}
set font_metrics(Helvetica) {ascend 718 bbox {-166 -225 1000 931 } descend -207 fixed 0}
set font_metrics(Helvetica-Bold) {ascend 718 bbox {-170 -228 1003 962 } descend -207 fixed 0}
set font_metrics(Helvetica-BoldOblique) {ascend 718 bbox {-174 -228 1114 962 } descend -207 fixed 0}
set font_metrics(Helvetica-Oblique) {ascend 718 bbox {-170 -225 1116 931 } descend -207 fixed 0}
set font_metrics(NewCenturySchlbk-Bold) {ascend 737 bbox {-166 -221 1000 1007} descend -205 fixed 0}
set font_metrics(NewCenturySchlbk-BoldItalic) {ascend 737 bbox {-170 -220 1151 990} descend -205 fixed 0}
set font_metrics(NewCenturySchlbk-Italic) {ascend 737 bbox {-166 -227 1018 968} descend -205 fixed 0}
set font_metrics(NewCenturySchlbk-Roman) {ascend 737 bbox {-217 -215 1000 980} descend -205 fixed 0}
set font_metrics(Palatino-Bold) {ascend 720 bbox {-152 -266 1000 924} descend -258 fixed 0}
set font_metrics(Palatino-BoldItalic) {ascend 726 bbox {-170 -271 1073 926} descend -271 fixed 0}
set font_metrics(Palatino-Italic) {ascend 733 bbox {-170 -276 1010 918} descend -276 fixed 0}
set font_metrics(Palatino-Roman) {ascend 726 bbox {-166 -283 1021 927} descend -281 fixed 0}
set font_metrics(Symbol) {ascend 0 bbox {-180 -293 1090 1010 } descend 0 fixed 0}
set font_metrics(Times-Bold) {ascend 683 bbox {-168 -218 1000 935 } descend -217 fixed 0}
set font_metrics(Times-BoldItalic) {ascend 683 bbox {-200 -218 996 921 } descend -217 fixed 0}
set font_metrics(Times-Italic) {ascend 683 bbox {-169 -217 1010 883 } descend -217 fixed 0}
set font_metrics(Times-Roman) {ascend 683 bbox {-168 -218 1000 898 } descend -217 fixed 0}
set font_metrics(ZapfChancery-MediumItalic) {ascend 714 bbox {-181 -314 1065 831} descend -314 fixed 0}
set font_metrics(ZapfDingbats) {ascend 0 bbox {-1 -143 981 820 } descend 0 fixed 0}
}
# library of tcl procedures for generating portable document format files
# this is a port of pdf4php from php to tcl

# Copyright (c) 2004 by Frank Richter <frichter@truckle.in-chemnitz.de> and
#                       Jens Pönisch <jens@ruessel.in-chemnitz.de>
# Copyright (c) 2006-2008 by Peter Spjuth <peter.spjuth@gmail.com>

# See the file "licence.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# $Id: pdf4tcl.tcl 163 2009-01-04 17:15:35Z pspjuth $

package provide pdf4tcl 0.5

package require pdf4tcl::metrics
package require pdf4tcl::glyphnames
package require snit

namespace eval pdf4tcl {
    # helper variables (constants) packaged into arrays to minimize
    # variable import statements
    variable g
    variable font_widths
    variable font_metrics
    variable glyph_names
    variable font_afm
    variable paper_sizes
    variable units
    variable dir [file dirname [file join [pwd] [info script]]]

    # path to adobe afm files
    set g(ADOBE_AFM_PATH) {}
    # change this to reflect your machines install!
    lappend g(ADOBE_AFM_PATH) {/usr/share/texmf/fonts/afm/adobe/*}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/enscript}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/ghostscript}
    lappend g(ADOBE_AFM_PATH) {/usr/share/fonts/default/Type1}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/a2ps/afm}
    #lappend g(ADOBE_AFM_PATH) {/usr/share/ogonkify/afm}
    lappend g(ADOBE_AFM_PATH) /usr/X11R6/lib/X11/fonts/Type1
    lappend g(ADOBE_AFM_PATH) /usr/lib/openoffice/share/psprint/fontmetric

    # font width array
    array set font_widths {}
    array set font_metrics {}

    # font name to afm file mapping array
    array set font_afm {}

    # Known papersizes. Make sure they have a unit to be independent of
    # default unit setting in a document.
    array set paper_sizes {
        a0     {2380p 3368p}
        a1     {1684p 2380p}
        a2     {1190p 1684p}
        a3     { 842p 1190p}
        a4     { 595p  842p}
        a5     { 421p  595p}
        a6     { 297p  421p}
        11x17  { 792p 1224p}
        ledger {1224p  792p}
        legal  { 612p 1008p}
        letter { 612p  792p}
    }

    # Known units. The value is their relationship to points
    array set units [list              mm [expr {72.0 / 25.4}]              m  [expr {72.0 / 25.4}]              cm [expr {72.0 / 2.54}]              c  [expr {72.0 / 2.54}]              i  72.0                              p  1.0                              ]

    if {[catch {package require zlib} err]} {
        set g(haveZlib) 0
    } else {
        set g(haveZlib) 1
    }

    proc Init {} {
        LoadAfmMapping
    }

    proc LoadAfmMapping {} {
        variable font_afm
        variable g

        foreach path $g(ADOBE_AFM_PATH) {
            foreach file [glob -nocomplain [file join $path "*.afm"]] {
                set if [open $file "r"]
                while {[gets $if line]!=-1} {
                    if {[regexp {^FontName\s*(.*)$} $line dummy fontname]} {
                        set font_afm($fontname) $file
                        break
                    }
                }
                close $if
            }
        }
        #parray font_afm
        #puts [join [lsort -dict [array names ::pdf4tcl::font_widths]] \n]
        #exit
    }

    # Utility to look up paper size by name
    # A two element list of width and height is also allowed.
    # Return value is in points
    proc getPaperSize {papername} {
        variable paper_sizes

        set papername [string tolower $papername]
        if {[info exists paper_sizes($papername)]} {
            set papername $paper_sizes($papername)
        }
        if {[catch {set len [llength $papername]}] || $len != 2} {
            return {}
        }
        foreach {w h} $papername break
        set w [getPoints $w]
        set h [getPoints $h]
        return [list $w $h]
    }

    # Return a list of known paper sizes
    proc getPaperSizeList {} {
        variable paper_sizes
        return [array names paper_sizes]
    }

    # Get points from a measurement.
    # No unit means points.
    # Supported units are "mm", "m", "cm", "c", "p" and "i".
    proc getPoints {val} {
        variable units
        if {[string is double -strict $val]} {
            # Always return a pure double value
            return [expr {$val * 1.0}]
        }
        if {[regexp {^\s*(\S+?)\s*([[:alpha:]]+)\s*$} $val -> num unit]} {
            if {[string is double -strict $num]} {
                if {[info exists units($unit)]} {
                    return [expr {$num * $units($unit)}]
                }
            }
        }
        return -code error "Unknown value $val"
    }

    # Wrapper to create pdf4tcl object
    proc new {args} {
        uplevel 1 pdf4tcl::pdf4tcl create $args
    }

    Init
}

# Object used for generating pdf
snit::type pdf4tcl::pdf4tcl { ##nagelfar nocover
    variable pdf

    #######################################################################
    # Global option handling
    #######################################################################

    option -file      -default "" -readonly 1
    option -paper     -default a4     -validatemethod CheckPaper              -configuremethod SetPageOption
    option -landscape -default 0      -validatemethod CheckBoolean              -configuremethod SetPageOption
    option -orient    -default 1      -validatemethod CheckBoolean
    option -unit      -default p      -validatemethod CheckUnit              -configuremethod SetUnit -readonly 1
    option -compress  -default 0      -validatemethod CheckBoolean              -configuremethod SetCompress -readonly 1
    option -margin    -default 0      -validatemethod CheckMargin              -configuremethod SetPageOption

    # Validator for -paper
    method CheckPaper {option value} {
        set papersize [pdf4tcl::getPaperSize $value]
        if {[llength $papersize] == 0} {
            return -code error "papersize $value is unknown"
        }
    }

    # Validator for -unit
    method CheckUnit {option value} {
        if {![info exists ::pdf4tcl::units($value)]} {
            return -code error "unit $value is unknown"
        }
    }

    # Validator for -margin
    method CheckMargin {option value} {
        switch [llength $value] {
            1 - 2 - 4 {
                foreach elem $value {
                    if {[catch {pdf4tcl::getPoints $elem}]} {
                        return -code error "Bad margin value '$elem'"
                    }
                }
            }
            default {
                return -code error "Bad margin list '$value'"
            }
        }
    }

    # Validator for boolean options
    method CheckBoolean {option value} {
        if {![string is boolean -strict $value]} {
            return -code error "option $option must have a boolean value."
        }
    }

    # Configure method for -compress
    method SetCompress {option value} {
        variable ::pdf4tcl::g
        if {$value} {
            if {$g(haveZlib)} {
                set options($option) 1
            } else {
                puts stderr "Package zlib not available. Sorry, no compression."
            }
        } else {
            set options($option) 0
        }
    }

    # Configure method for page properties
    method SetPageOption {option value} {
        set options($option) $value
        # Fill in page properies
        $self SetPageSize   $options(-paper) $options(-landscape)
        $self SetPageMargin $options(-margin)
    }

    # Configure method for -unit
    method SetUnit {option value} {
        set options($option) $value
        set pdf(unit) $::pdf4tcl::units($value)
    }

    #######################################################################
    # Constructor
    #######################################################################

    constructor {args} {
        variable images
        variable fonts
        variable bitmaps
        variable patterns

        # The unit translation factor is needed before parsing arguments
        set pdf(unit) 1.0

        $self configurelist $args

        # Document data
        set pdf(pages) {}
        set pdf(pdf_obj) 4 ;# Objects 1-3 are reserved for use in "finish"
        set pdf(out_pos) 0
        set pdf(data_start) 0
        set pdf(data_len) 0
        array set fonts {}
        set pdf(font_set) false
        set pdf(in_text_object) false
        array set images {}
        array set bitmaps {}
        array set patterns {}
        set pdf(objects) {}
        set pdf(compress) $options(-compress)
        set pdf(finished) false
        set pdf(inPage) false
        set pdf(fillColor) [list 0 0 0]
        # start without default font
        set pdf(font_size) 1
        set pdf(current_font) ""
        set pdf(line_spacing) 1.0

        # Page data
        # Fill in page properies
        $self SetPageSize   $options(-paper) $options(-landscape)
        $self SetPageMargin $options(-margin)
        set pdf(orient) $options(-orient)

        # The first buffer if for collecting page data until end of page.
        # This is to support compressing whole pages.
        set pdf(ob) ""

        # Write to file directly if requested.
        set pdf(ch) ""
        if {$options(-file) ne ""} {
            if {[catch {open $options(-file) "w"} ch]} {
                return -code error "Could not open file $options(-file) for writing: $ch"
            }
            fconfigure $ch -translation binary
            set pdf(ch) $ch
        }

        # collect output in memory
        set pdf(pdf) ""

        # Start on pdfout
        $self Pdfout "%PDF-1.4\n"
        set pdf(version) 1.4
        # Add some chars >= 0x80 as recommended by the PDF standard
        # to make it easy to detect that this is not an ASCII file.
        $self Pdfout "%\xE5\xE4\xF6\n"
    }

    destructor {
        # Close any open channel
        if {[info exists pdf(ch)] && $pdf(ch) ne ""} {
            catch {$self finish}
            catch {close $pdf(ch)}
            set pdf(ch) ""
        }
    }

    # Deprecated destroy function
    method cleanup {} {
        $self destroy
    }


    #######################################################################
    # Collect PDF Output
    #######################################################################

    # Add raw data to accumulated pdf output
    method Pdfout {out} {
        append pdf(ob) $out
        incr pdf(out_pos) [string length $out]
    }

    # Add line of words to accumulated pdf output
    method Pdfoutn {args} {
        set out [join $args " "]\n
        $self Pdfout $out
    }

    # Helper to format a line consisiting of numbers and last a command
    method Pdfoutcmd {args} {
        set str ""
        foreach num [lrange $args 0 end-1] {
            append str [Nf $num] " "
        }
        append str "[lindex $args end]\n"
        $self Pdfout $str
    }

    # Move data from pdf(ob) cache to final destination.
    # Return number of bytes added
    method Flush {{compress 0}} {
        set data $pdf(ob)
        set pdf(ob) ""
        if {$compress} {
            set data [zlib compress $data]
        }
        set len [string length $data]
        if {$pdf(ch) eq ""} {
            append pdf(pdf) $data
        } else {
            puts -nonewline $pdf(ch) $data
        }
        return $len
    }

    #######################################################################
    # ?? Handling
    #######################################################################

    # Utility to look up paper size by name
    # A two element list of width and height is also allowed.
    method GetPaperSize {papername} {
        variable ::pdf4tcl::paper_sizes

        set papername [string tolower $papername]
        if {[info exists paper_sizes($papername)]} {
            set papername $paper_sizes($papername)
        }
        if {[catch {set len [llength $papername]}] || $len != 2} {
            return {}
        }
        foreach {w h} $papername break
        set w [$self GetPoints $w]
        set h [$self GetPoints $h]
        return [list $w $h]
    }

    # Get points from a measurement.
    # No unit means: use current unit setting
    # Supported units are "mm", "m", "cm", "c", "p" and "i".
    method GetPoints {val} {
        variable ::pdf4tcl::units
        if {[string is double -strict $val]} {
            # Always return a pure double value
            return [expr {$val * $pdf(unit)}]
        }
        if {[regexp {^\s*(\S+?)\s*([[:alpha:]]+)\s*$} $val -> num unit]} {
            if {[string is double -strict $num]} {
                if {[info exists units($unit)]} {
                    return [expr {$num * $units($unit)}]
                }
            }
        }
        return -code error "Unknown value $val"
    }

    # If any feature requires PDF version > 1.4 they should call this
    method RequireVersion {version} {
        if {$version > $pdf(version)} {
            set pdf(version) $version
        }
    }

    #######################################################################
    # Page Handling
    #######################################################################

    # Fill in page margin from a user specified value
    method SetPageMargin {value} {
        switch -- [llength $value] {
            1 {
                set pdf(marginleft)   [$self GetPoints [lindex $value 0]]
                set pdf(marginright)  [$self GetPoints [lindex $value 0]]
                set pdf(margintop)    [$self GetPoints [lindex $value 0]]
                set pdf(marginbottom) [$self GetPoints [lindex $value 0]]
            }
            2 {
                set pdf(marginleft)   [$self GetPoints [lindex $value 0]]
                set pdf(marginright)  [$self GetPoints [lindex $value 0]]
                set pdf(margintop)    [$self GetPoints [lindex $value 1]]
                set pdf(marginbottom) [$self GetPoints [lindex $value 1]]
            }
            4 {
                set pdf(marginleft)   [$self GetPoints [lindex $value 0]]
                set pdf(marginright)  [$self GetPoints [lindex $value 1]]
                set pdf(margintop)    [$self GetPoints [lindex $value 2]]
                set pdf(marginbottom) [$self GetPoints [lindex $value 3]]
            }
            default { ##nagelfar nocover
                # This should not happen since validation should catch it
                puts "ARARARARARAR '$value'"
            }
        }
    }

    # Fill in page data from options
    method SetPageSize {paper landscape} {
        set papersize [$self GetPaperSize $paper]
        set width  [lindex $papersize 0]
        set height [lindex $papersize 1]

        # Switch if landscape has been asked for
        if {$landscape} {
            set tmp    $width
            set width  $height
            set height $tmp
        }
        set pdf(width)  $width
        set pdf(height) $height
        set pdf(xpos)   0
        set pdf(ypos)   $height
    }

    # Start on a new page
    method startPage {args} {
        # Get defaults from document
        set localopts(-orient)    $options(-orient)
        set localopts(-landscape) $options(-landscape)
        set localopts(-margin)    $options(-margin)
        set localopts(-paper)     $options(-paper)

        if {[llength $args] == 1} {
            # Single arg = paper
            $self CheckPaper -paper [lindex $args 0]
            set localopts(-paper) [lindex $args 0]
        } elseif {[llength $args] == 2 && [string is digit [join $args ""]]} {
            # Old style two numeric args = {width height}
            $self CheckPaper -paper $args
            set localopts(-paper) $args
        } elseif {[llength $args] == 3 && [string is digit [join $args ""]]} {
            # Old style three numeric args = {width height orient}
            $self CheckPaper -paper [lrange $args 0 1]
            set localopts(-paper)   [lrange $args 0 1]
            set localopts(-orient)  [lindex $args 2]
        } elseif {[llength $args] % 2 != 0} {
            # Uneven, error
            return -code error "Uneven number of arguments to startPage"
        } else {
            # Parse options
            foreach {option value} $args {
                switch -- $option {
                    -paper {
                        $self CheckPaper $option $value
                    }
                    -landscape {
                        $self CheckBoolean $option $value
                    }
                    -margin {
                        $self CheckMargin $option $value
                    }
                    -orient {
                        $self CheckBoolean $option $value
                    }
                    default {
                        return -code error "Unknown option $option"
                    }
                }
                set localopts($option) $value
            }
        }

        if {$pdf(inPage)} {
            $self endPage
        }
        # Fill in page properies
        $self SetPageSize $localopts(-paper) $localopts(-landscape)
        $self SetPageMargin $localopts(-margin)
        set pdf(orient) $localopts(-orient)

        set pdf(inPage) 1

        # dimensions
        set oid [$self GetOid]
        lappend pdf(pages) $oid
        $self Pdfout "$oid 0 obj\n"
        $self Pdfout "<</Type /Page\n"
        $self Pdfout "/Parent 2 0 R\n"
        $self Pdfout "/Resources 3 0 R\n"
        $self Pdfout [format "/MediaBox \[0 0 %g %g\]\n" $pdf(width) $pdf(height)]
        $self Pdfout "/Contents \[[$self NextOid] 0 R \]\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        # start of contents
        set oid [$self GetOid]
        $self Pdfout "$oid 0 obj\n"
        # Allocate an object for the page length
        set pdf(pagelengthoid) [$self GetOid 1]
        $self Pdfout "<<\n/Length $pdf(pagelengthoid) 0 R\n"
        if {$pdf(compress)} {
            $self Pdfout "/Filter \[/FlateDecode\]\n"
        }
        $self Pdfout ">>\nstream\n"
        set pdf(data_start) $pdf(out_pos)
        set pdf(in_text_object) false

        # no font set on new pages
        set pdf(font_set) false

        # capture output
        $self Flush
    }

    # Finish a page
    method endPage {} {
        if {! $pdf(inPage)} {
            return
        }
        if {$pdf(in_text_object)} {
            $self Pdfout "\nET\n"
        }
        # get buffer
        set data_len [$self Flush $pdf(compress)]
        set pdf(out_pos) [expr {$pdf(data_start)+$data_len}]
        $self Pdfout "\nendstream\n"
        $self Pdfout "endobj\n\n"

        # Create Length object
        $self StoreXref $pdf(pagelengthoid)
        $self Pdfout "$pdf(pagelengthoid) 0 obj\n"
        incr data_len
        $self Pdfout "$data_len\n"
        $self Pdfout "endobj\n\n"
        set pdf(inPage) false

        # Dump stored objects
        $self FlushObjects
    }

    method FlushObjects {} {
        if {$pdf(inPage)} {
            return -code error "FlushObjects may not be called when in a page"
        }

        # Dump stored objects
        foreach {oid body} $pdf(objects) {
            $self StoreXref $oid
            $self Pdfout $body
        }
        set pdf(objects) {}
        $self Flush
    }

    # Create an object to be added to the stream at a suitable time.
    # Returns the Object Id.
    method AddObject {body} {
        set oid [$self GetOid 1]
        lappend pdf(objects) $oid "$oid 0 obj\n$body\nendobj\n"
        return $oid
    }

    # Finish document
    method finish {} {
        variable images
        variable patterns
        variable fonts

        if {$pdf(finished)} {
            return
        }

        if {$pdf(inPage)} {
            $self endPage
        }
        # Object 1 is the Root of the document
        $self StoreXref 1
        $self Pdfout "1 0 obj\n"
        $self Pdfout "<<\n"
        $self Pdfout "/Type /Catalog\n"
        if {$pdf(version) > 1.4} {
            $self Pdfout "/Version $pdf(version)\n"
        }
        $self Pdfout "/Pages 2 0 R\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        # Object 2 lists the pages
        $self StoreXref 2
        $self Pdfout "2 0 obj\n"
        $self Pdfout "<<\n/Type /Pages\n"
        $self Pdfout "/Count [llength $pdf(pages)]\n"
        $self Pdfout "/Kids \["
        foreach oid $pdf(pages) {
            $self Pdfout "$oid 0 R "
        }
        $self Pdfout "\]\n"
        $self Pdfout ">>\n"
        $self Pdfout "endobj\n\n"

        # Object 3 is the Resources Object
        $self StoreXref 3
        $self Pdfout "3 0 obj\n"
        $self Pdfout "<<\n"
        $self Pdfout "/ProcSet\[/PDF /Text /ImageC\]\n"

        # font references
        if {[array size fonts] > 0} {
            $self Pdfout "/Font <<\n"
            foreach fontname [array names fonts] {
                set oid $fonts($fontname)
                $self Pdfout "/$fontname $oid 0 R\n"
            }
            $self Pdfout ">>\n"
        }

        # image references
        if {[array size images] > 0} {
            $self Pdfout "/XObject <<\n"
            foreach key [array names images] {
                set oid [lindex $images($key) 2]
                $self Pdfout "/$key $oid 0 R\n"
            }
            $self Pdfout ">>\n"
        }
        # pattern references
        if {[array size patterns] > 0} {
            $self Pdfout "/ColorSpace <<\n"
            $self Pdfout "/Cs1 \[/Pattern /DeviceRGB\]\n"
            $self Pdfout ">>\n"

            $self Pdfout "/Pattern <<\n"
            foreach key [array names patterns] {
                set oid [lindex $patterns($key) 2]
                $self Pdfout "/$key $oid 0 R\n"
            }
            $self Pdfout ">>\n"
        }

        $self Pdfout ">>\nendobj\n\n"

        # Cross reference table
        set xref_pos $pdf(out_pos)
        $self Pdfout "xref\n"
        $self Pdfout "0 [$self NextOid]\n"
        $self Pdfout "0000000000 65535 f \n"
        for {set a 1} {$a<[$self NextOid]} {incr a} {
            set xref $pdf(xref,$a)
            $self Pdfout [format "%010ld 00000 n \n" $xref]
        }

        # Document trailer
        $self Pdfout "trailer\n"
        $self Pdfout "<<\n"
        $self Pdfout "/Size [$self NextOid]\n"
        $self Pdfout "/Root 1 0 R\n"
        $self Pdfout ">>\n"
        $self Pdfout "\nstartxref\n"
        $self Pdfout "$xref_pos\n"
        $self Pdfout "%%EOF\n"
        $self Flush
        set pdf(finished) true
    }

    # Get finished PDF data
    method get {} {
        if {$pdf(inPage)} {
            $self endPage
        }
        if {! $pdf(finished)} {
            $self finish
        }
        return $pdf(pdf)
    }

    # Write PDF data to file
    method write {args} {
        set chan stdout
        set outfile 0
        foreach {arg value} $args {
            switch -- $arg {
                "-file" {
                    if {[catch {open $value "w"} chan]} {
                        return -code error "Could not open file $value for writing: $chan"
                    } else {
                        set outfile 1
                    }
                }
                default {
                    return -code error "unknown option $arg."
                }
            }
        }

        fconfigure $chan -translation binary
        puts -nonewline $chan [$self get]
        if {$outfile} {
            close $chan
        }
    }

    # Transform absolute user coordinates to page coordinates
    # This should take into account orientation, margins.
    method Trans {x y txName tyName} {
        upvar 1 $txName tx $tyName ty

        set px [$self GetPoints $x]
        set py [$self GetPoints $y]

        set tx [expr {$px + $pdf(marginleft)}]
        if {$pdf(orient)} {
            set ty [expr {$py + $pdf(margintop)}]
            set ty [expr {$pdf(height) - $ty}]
        } else {
            set ty [expr {$py + $pdf(marginbottom)}]
        }
    }

    # Transform relative user coordinates to page coordinates
    # This should take into account orientation, but not margins.
    method TransR {x y txName tyName} {
        upvar 1 $txName tx $tyName ty

        set tx [$self GetPoints $x]
        set ty [$self GetPoints $y]

        if {$pdf(orient)} {
            set ty [expr {- $ty}]
        }
    }

    # Returns width and height of drawable area, excluding margins.
    method getDrawableArea {} {
        set w [expr {$pdf(width)  - $pdf(marginleft) - $pdf(marginright)}]
        set h [expr {$pdf(height) - $pdf(margintop)  - $pdf(marginbottom)}]
        # Translate to current unit
        set w [expr {$w / $pdf(unit)}]
        set h [expr {$h / $pdf(unit)}]
        return [list $w $h]
    }

    #######################################################################
    # Text Handling
    #######################################################################

    # Set current font
    method setFont {size {fontname ""} {internal 0}} {
        variable ::pdf4tcl::font_widths

        if {$fontname eq ""} {
            if {$pdf(current_font) eq ""} {
                return -code error "No font family set"
            }
            set fontname $pdf(current_font)
        }
        # font width already loaded?
        if {! [info exists font_widths($fontname)]} {
            if {[catch {loadFontMetrics $fontname} tmp]} {
                return -code error "Could not load font metrics for $fontname"
            } else {
                set font_widths($fontname) $tmp
            }
        }

        if {!$internal} {
            set size [$self GetPoints $size]
        }

        set pdf(current_font) $fontname
        set pdf(font_size) $size

        # Delay putting things in until we are actually on a page
        if {$pdf(inPage)} {
            $self SetupFont
        }
    }

    # Set the current font on the page
    method SetupFont {} {
        variable fonts
        
        if {$pdf(current_font) eq ""} {
            return -code error "No font set"
        }
        set fontname $pdf(current_font)
        $self Pdfoutn "/$fontname [Nf $pdf(font_size)]" "Tf"
        $self Pdfoutcmd 0 "Tr"
        $self Pdfoutcmd $pdf(font_size) "TL"

        # Make sure a font object exists
        if {![info exists fonts($pdf(current_font))]} {
            set body    "<<\n/Type /Font\n"
            append body "/Subtype /Type1\n"
            append body "/Encoding /WinAnsiEncoding\n"
            append body "/Name /$fontname\n"
            append body "/BaseFont /$fontname\n"
            append body ">>"
            set oid [$self AddObject $body]
            set fonts($pdf(current_font)) $oid
        }
        set pdf(font_set) true
    }

    # Load font metrics from AFM file
    proc loadFontMetrics {font} {
        variable ::pdf4tcl::font_afm

        set file $font_afm($font)
        if {[catch {open $file "r"} if]} {
            return ""
        } else {
            set started false
            array set widths {}
            while {[gets $if line]!=-1} {
                if {! $started} {
                    if {[string first "StartCharMetrics" $line]==0} {
                        set started true
                    }
                } else {
                    # Done?
                    if {[string first "EndCharMetrics" $line]==0} {
                        break
                    }
                    if {[string index $line 0]=="C"} {
                        scan [string range $line 1 4] "%d" ch
                        if {($ch>0) && ($ch<256)} {
                            set pos [string first "WX" $line]
                            incr pos 2
                            set endpos $pos
                            incr endpos 4
                            scan [string range $line $pos $endpos] "%d" w
                            set char [format "%c" $ch]
                            set widths($char) $w
                        }
                    }
                }
            }
            close $if
            return [array get widths]
        }
    }

    # Get metrics from current font.
    # Supported metrics are ascend, descend, fixed, bboxy, height
    method getFontMetric {metric {internal 0}} {
        if {$pdf(current_font) eq ""} {
            return -code error "No font set"
        }
        array set tmp $::pdf4tcl::font_metrics($pdf(current_font))
        switch $metric {
            bboxy   {set val [expr {[lindex $tmp(bbox) 1] * 0.001}]}
            fixed   {return $tmp(fixed)}
            height  {set val 1.0}
            default {set val [expr {$tmp($metric) * 0.001}]}
        }
        # Translate to current unit
        if {!$internal} {
            set val [expr {$val/ $pdf(unit)}]
        }
        return [expr {$val * $pdf(font_size)}]
    }

    # Get the width of a string under the current font.
    method getStringWidth {txt {internal 0}} {
        if {$pdf(current_font) eq ""} {
            return -code error "No font set"
        }
        set w 0.0
        foreach ch [split $txt ""] {
            set w [expr {$w + [GetCharWidth $pdf(current_font) $ch]}]
        }
        if {!$internal} {
            set w [expr {$w / $pdf(unit)}]
        }
        return [expr {$w * $pdf(font_size)}]
    }

    # Get the width of a character. "ch" must be exacly one char long.
    # This is a proc for performance reasons since it is called a lot.
    # Currently this is four times slower as a method.
    # With a method it would be preferable to keep the cache in
    # the instance to clean things up.
    proc GetCharWidth {font ch} {
        if {[info exists ::pdf4tcl::FontWidthsCh($font,$ch)]} {
            return $::pdf4tcl::FontWidthsCh($font,$ch)
        }

        if {$ch eq "\n"} {
            set res 0.0
            set ::pdf4tcl::FontWidthsCh($font,$ch) $res
            return $res
        }

        if {![info exists ::pdf4tcl::FontWidthsCurrent] ||                  $::pdf4tcl::FontWidthsCurrent ne $font} {
            array unset ::pdf4tcl::FontWidths
            array set ::pdf4tcl::FontWidths $::pdf4tcl::font_widths($font)
            set ::pdf4tcl::FontWidthsCurrent $font
        }

        # This can't fail since ch is always 1 char long
        scan $ch %c n

        set ucs2 [format "%04.4X" $n]

        set glyph_name zero
        set w 0
        catch {set w $::pdf4tcl::FontWidths(zero)}
        catch {set glyph_name $::pdf4tcl::glyph_names($ucs2)}
        if {$glyph_name eq "spacehackarabic"} {set glyph_name "space"}

        catch {set w $::pdf4tcl::FontWidths($glyph_name)}
        ###puts stderr "ch: $ch  n: $n  ucs2: $ucs2  glyphname: $glyph_name  width: $w"
        set res [expr {$w * 0.001}]
        set ::pdf4tcl::FontWidthsCh($font,$ch) $res
        return $res
    }

    # Get the width of a character under the current font.
    method getCharWidth {ch {internal 0}} {
        if {$pdf(current_font) eq ""} {
            return -code error "No font set"
        }
        set len [string length $ch]
        if {$len == 0} {
            return 0.0
        } elseif {$len > 1} {
            set ch [string index $ch 0]
        }
        set width [expr {[GetCharWidth $pdf(current_font) $ch] * $pdf(font_size)}]
        if {!$internal} {
            set width [expr {$width / $pdf(unit)}]
        }
        return $width
    }

    # Set coordinate for next text command. Internal version
    method SetTextPosition {x y} {
        $self BeginTextObj
        set pdf(xpos) $x
        set pdf(ypos) $y
        $self Pdfoutcmd 1 0 0 1 $pdf(xpos) $pdf(ypos) "Tm"
    }

    method SetTextPositionAngle {x y angle xangle yangle} {
        $self BeginTextObj
        set rad [expr {$angle*3.1415926/180.0}]
        set c [expr {cos($rad)}]
        set s [expr {sin($rad)}]
        set pdf(xpos) $x
        set pdf(ypos) $y
        $self Pdfoutcmd $c [expr {-$s}] $s $c $pdf(xpos) $pdf(ypos) "Tm"

        # TODO: support skew too
        set tx [expr {tan($xangle*3.1415926/180.0)}]
        set ty [expr {tan($yangle*3.1415926/180.0)}]
        #$self Pdfoutcmd 1 $tx $ty 1 $pdf(xpos) $pdf(ypos) "Tm"
    }

    # Set coordinate for next text command.
    method setTextPosition {x y} {
        $self BeginTextObj
        $self Trans $x $y x y
        # Store for reference
        set pdf(origxpos) $x
        set pdf(origypos) $y
        $self SetTextPosition $x $y
    }

    # Move coordinate for next text command.
    method moveTextPosition {x y} {
        $self TransR $x $y x y
        set y [expr {$pdf(ypos) + $y}]
        set x [expr {$pdf(xpos) + $x}]
        $self SetTextPosition $x $y
    }

    # Draw text at current position, with a newline before
    # DEPRECATED!
    method drawText {str} {
        $self BeginTextObj
        if {! $pdf(font_set)} {
            $self SetupFont
        }
        $self Pdfout "([CleanText $str]) '\n"
        # Update to next line
        set strWidth [$self getStringWidth $str 1]
        set pdf(ypos) [expr {$pdf(ypos) - $pdf(font_size) * $pdf(line_spacing)}]
        set pdf(xpos) [expr {$pdf(origxpos) + $strWidth}]
    }

    # Move text position to new line, relative to last
    # setTextPosition command.
    method newLine {{spacing {}}} {
        if {$spacing eq ""} {
            set spacing $pdf(line_spacing)
        } elseif {![string is double -strict $spacing]} {
            return -code error "Line spacing must be a number"
        }
        # Update to next line
        set y [expr {$pdf(ypos) - $pdf(font_size) * $spacing}]
        set x $pdf(origxpos)
        $self SetTextPosition $x $y
    }

    # Set Line spacing factor (which is used by method newLine
    # if no explicit spacing is given)
    method setLineSpacing {spacing} {
        if {![string is double -strict $spacing]} {
            return -code error "Line spacing must be a number"
        }
        set pdf(line_spacing) $spacing
    }

    # Return the current line spacing factor
    method getLineSpacing {} {
        return $pdf(line_spacing)
    }

    # Draw a text string
    # Returns the width of the drawn string.
    method text {str args} {
        if {!$pdf(inPage)} { $self startPage }
        set align "left"
        set angle 0
        set bg 0
        set x $pdf(xpos)
        set y $pdf(ypos)
        set posSet 0

        foreach {arg value} $args {
            switch -- $arg {
                "-align" {
                    set align $value
                }
                "-angle" {
                    set angle $value
                }
                "-background" - "-bg" - "-fill" {
                    if {[string is boolean -strict $value]} {
                        set bg $value
                    } else {
                        set bg [GetColor $value]
                    }
                }
                "-y" {
                    $self Trans 0 $value _ y
                    set posSet 1
                }
                "-x" {
                    $self Trans $value 0 x _
                    set posSet 1
                }
                default {
                    return -code error                              "unknown option $arg"
                }
            }
        }

        if {! $pdf(font_set)} {
            $self SetupFont
        }

        set strWidth [$self getStringWidth $str 1]
        if {$align == "right"} {
            set x [expr {$x - $strWidth}]
            set posSet 1
        } elseif {$align == "center"} {
            set x [expr {$x - $strWidth / 2 * cos($angle*3.1415926/180.0)}]
            set y [expr {$y - $strWidth / 2 * sin($angle*3.1415926/180.0)}]
            set posSet 1
        }
        # Draw a background box if needed.
        if {[llength $bg] > 1 || $bg} {
            set bboxy [$self getFontMetric bboxy 1]
            set dy [expr {$y + $bboxy}]
            $self EndTextObj
            # Temporarily shift fill color
            $self Pdfoutcmd "q"
            if {[llength $bg] > 1} {
                $self Pdfout "$bg rg\n"
            } else {
                $self Pdfout "$pdf(bgColor) rg\n"
            }
            $self DrawRect $x $dy $strWidth $pdf(font_size) 0 1
            $self Pdfoutcmd "Q"
            # Position needs to be set since we left the text object
            set posSet 1
        }
        $self BeginTextObj
        if {$angle != 0} {
            $self SetTextPositionAngle $x $y $angle 0 0
        } elseif {$posSet} {
            $self SetTextPosition $x $y
        }
        $self Pdfout "([CleanText $str]) Tj\n"
        set pdf(xpos) [expr {$x + $strWidth}]
        return $strWidth
    }

    # Draw a text string at a given position.
    method DrawTextAt {x y str {align left}} {
        if {! $pdf(font_set)} {
            $self SetupFont
        }

        set strWidth [$self getStringWidth $str 1]
        if {$align == "right"} {
            set x [expr {$x - $strWidth}]
        } elseif {$align == "center"} {
            set x [expr {$x - $strWidth / 2}]
        }
        $self BeginTextObj
        $self SetTextPosition $x $y
        $self Pdfout "([CleanText $str]) Tj\n"
    }

    method drawTextBox {x y width height txt args} {
        if {!$pdf(inPage)} { $self startPage }
        set align left
        set linesVar ""
        foreach {arg value} $args {
            switch -- $arg {
                "-align" {
                    set align $value
                }
                "-linesvar" {
                    set linesVar $value
                }
                default {
                    return -code error                              "unknown option $arg"
                }
            }
        }

        if {$linesVar ne ""} {
            upvar 1 $linesVar lines
        }
        set lines 0

        $self Trans  $x $y x y
        $self TransR $width $height width height

        if {!$pdf(orient)} {
            # Always have anchor position upper left
            set y [expr {$y + $height}]
        } else {
            # Restore a positive height
            set height [expr {- $height}]
        }

        $self BeginTextObj
        if {! $pdf(font_set)} {
            $self SetupFont
        }

        # pre-calculate some values
        set font_height [expr {$pdf(font_size) * $pdf(line_spacing)}]
        set space_width [$self getCharWidth " " 1]

        # Displace y to put the first line within the box
        set bboxy [$self getFontMetric bboxy 1]
        set ystart $y
        set y [expr {$y - $pdf(font_size) - $bboxy}]

        set len [string length $txt]

        # run through chars until we exceed width or reach end
        set start 0
        set pos 0
        set cwidth 0
        set lastbp 0
        set done false

        while {! $done} {
            set ch [string index $txt $pos]
            # test for breakable character
            if {[regexp "\[ \t\r\n-\]" $ch]} {
                set lastbp $pos
            }
            set w [$self getCharWidth $ch 1]
            if {($cwidth+$w)>$width || $pos>=$len || $ch=="\n"} {
                if {$pos>=$len} {
                    set done true
                } else {
                    # backtrack to last breakpoint
                    if {$lastbp != $start} {
                        set pos $lastbp
                    } else {
                        # Word longer than line.
                        # Back up one char if possible
                        if {$pos > $start} {
                            incr pos -1
                        }
                    }
                }
                set sent [string trim [string range $txt $start $pos]]
                switch -- $align {
                    "justify" {
                        # count number of spaces
                        set words [split $sent " "]
                        if {[llength $words]>1 && (!$done) && $ch!="\n"} {
                            # determine additional width per space
                            set sw [$self getStringWidth $sent 1]
                            set add [expr {($width-$sw)/([llength $words]-1)}]
                            # display words
                            $self Pdfoutcmd $add "Tw"
                            $self DrawTextAt $x $y $sent
                            $self Pdfoutcmd 0 "Tw"
                        } else {
                            $self DrawTextAt $x $y $sent
                        }
                    }
                    "right" {
                        $self DrawTextAt [expr {$x+$width}] $y $sent right
                    }
                    "center" {
                        $self DrawTextAt [expr {$x+$width/2.0}] $y $sent center
                    }
                    default {
                        $self DrawTextAt $x $y $sent
                    }
                }
                # Move y down to next line
                set y [expr {$y-$font_height}]
                incr lines

                set start $pos
                incr start
                set cwidth 0
                set lastbp $start

                # Will another line fit?
                if {($ystart - ($y + $bboxy)) > $height} {
                    return [string range $txt $start end]
                }
            } else {
                set cwidth [expr {$cwidth+$w}]
            }
            incr pos
        }
        return ""
    }

    # start text object, if not already in text
    method BeginTextObj {} {
        if {! $pdf(in_text_object)} {
            $self Pdfout "BT\n"
            set pdf(in_text_object) true
        }
    }

    # end text object, if in text, else do nothing
    method EndTextObj {} {
        if {!$pdf(inPage)} { $self startPage }
        if {$pdf(in_text_object)} {
            $self Pdfout "ET\n"
            set pdf(in_text_object) false
        }
    }

    #######################################################################
    # Graphics Handling
    #######################################################################

    # Convert any user color to PDF color
    proc GetColor {color} {
        # Remove list layers, to accept things that have been 
        # multiply listified
        if {[llength $color] == 1} {
            set color [lindex $color 0]
        }
        if {[llength $color] == 3} {
            # Maybe range check them here...
            return $color
        }
        if {[regexp {^\#([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})$}                  $color -> rHex gHex bHex]} {
            set red   [expr {[scan $rHex %x] / 255.0}]
            set green [expr {[scan $gHex %x] / 255.0}]
            set blue  [expr {[scan $bHex %x] / 255.0}]
            return [list $red $green $blue]
        }
        # Use catch both to catch bad color, and to catch Tk not present
        if {[catch {winfo rgb . $color} tkcolor]} {
            return -code error "Unknown color: $color"
        }
        foreach {red green blue} $tkcolor break
        set red   [expr {($red   & 0xFF00) / 65280.0}]
        set green [expr {($green & 0xFF00) / 65280.0}]
        set blue  [expr {($blue  & 0xFF00) / 65280.0}]
        return [list $red $green $blue]

    }

    ###<jpo 2004-11-08: replaced "on off" by "args"
    ###                 to enable resetting dashed lines
    method setLineStyle {width args} {
        $self EndTextObj
        $self Pdfoutcmd $width "w"
        $self Pdfout "\[$args\] 0 d\n"
    }

    method DrawLine {args} {
        $self EndTextObj
        set cmd "m"
        foreach {x y} $args {
            $self Pdfoutcmd $x $y $cmd
            set cmd "l"
        }
        $self Pdfoutcmd "S"
    }

    method line {x1 y1 x2 y2} {
        if {!$pdf(inPage)} { $self startPage }
        $self Trans $x1 $y1 x1 y1
        $self Trans $x2 $y2 x2 y2

        $self DrawLine $x1 $y1 $x2 $y2
    }

    ###>2004-11-03 jpo
    method qCurve {x1 y1 xc yc x2 y2} {
        $self EndTextObj
        $self Trans $x1 $y1 x1 y1
        $self Trans $xc $yc xc yc
        $self Trans $x2 $y2 x2 y2
        $self Pdfoutcmd $x1 $y1 "m"
        $self Pdfoutcmd                  [expr {0.3333*$x1+0.6667*$xc}]                  [expr {0.3333*$y1+0.6667*$yc}]                  [expr {0.3333*$x2+0.6667*$xc}]                  [expr {0.3333*$y2+0.6667*$yc}]                  $x2                  $y2 "c"
        $self Pdfoutcmd "S"
    }
    ###<jpo

    # Draw a polygon
    method polygon {args} {
        $self EndTextObj

        set filled 0
        set stroke 1
        set start 1
        foreach {x y} $args {
            if {[string match {-[a-z]*} $x]} {
                switch -- $x {
                    "-filled" {
                        set filled $y
                    }
                    "-stroke" {
                        set stroke $y
                    }
                    default {
                        return -code error "unknown option $x"
                    }
                }
            } else {
                $self Trans $x $y x y
                if {$start} {
                    $self Pdfoutcmd $x $y "m"
                    set start 0
                } else {
                    $self Pdfoutcmd $x $y "l"
                }
            }
        }
        if {$filled && $stroke} {
            $self Pdfoutcmd "b"
        } elseif {$filled && !$stroke} {
            $self Pdfoutcmd "f"
        } else {
            $self Pdfoutcmd "s"
        }
    }

    method DrawOval {x y rx ry stroke filled} {
        $self EndTextObj

        set sq [expr {4.0*(sqrt(2.0)-1.0)/3.0}]
        set x0(0) [expr {$x+$rx}]
        set y0(0) $y
        set x1(0) [expr {$x+$rx}]
        set y1(0) [expr {$y+$ry*$sq}]
        set x2(0) [expr {$x+$rx*$sq}]
        set y2(0) [expr {$y+$ry}]
        set x3(0) $x
        set y3(0) [expr {$y+$ry}]
        set x1(1) [expr {$x-$rx*$sq}]
        set y1(1) [expr {$y+$ry}]
        set x2(1) [expr {$x-$rx}]
        set y2(1) [expr {$y+$ry*$sq}]
        set x3(1) [expr {$x-$rx}]
        set y3(1) $y
        set x1(2) [expr {$x-$rx}]
        set y1(2) [expr {$y-$ry*$sq}]
        set x2(2) [expr {$x-$rx*$sq}]
        set y2(2) [expr {$y-$ry}]
        set x3(2) $x
        set y3(2) [expr {$y-$ry}]
        set x1(3) [expr {$x+$rx*$sq}]
        set y1(3) [expr {$y-$ry}]
        set x2(3) [expr {$x+$rx}]
        set y2(3) [expr {$y-$ry*$sq}]
        set x3(3) [expr {$x+$rx}]
        set y3(3) $y
        $self Pdfoutcmd $x0(0) $y0(0) "m"
        for {set i 0} {$i < 4} {incr i} {
            $self Pdfoutcmd $x1($i)                              $y1($i)                              $x2($i)                              $y2($i)                              $x3($i)                              $y3($i) "c"
        }
        if {$filled && $stroke} {
            $self Pdfoutcmd "b"
        } elseif {$filled && !$stroke} {
            $self Pdfoutcmd "f"
        } else {
            $self Pdfoutcmd " s"
        }
    }

    method circle {x y r args} {
        if {!$pdf(inPage)} { $self startPage }
        set filled 0
        set stroke 1

        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled $value
                }
                "-stroke" {
                    set stroke $value
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }

        $self Trans $x $y x y
        set r [$self GetPoints $r]

        $self DrawOval $x $y $r $r $stroke $filled
    }

    method oval {x y rx ry args} {
        if {!$pdf(inPage)} { $self startPage }
        set filled 0
        set stroke 1

        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled $value
                }
                "-stroke" {
                    set stroke $value
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }

        $self Trans $x $y x y
        set rx [$self GetPoints $rx]
        set ry [$self GetPoints $ry]

        $self DrawOval $x $y $rx $ry $stroke $filled
    }

    # rotate by phi, scale with rx/ry and move by (dx, dy)
    proc Transform {rx ry phi dx dy points} {
        set cos_phi [expr {cos($phi)}]
        set sin_phi [expr {sin($phi)}]
        set res [list]
        foreach {x y} $points {
            set xn [expr {$rx * ($x*$cos_phi - $y*$sin_phi) + $dx}]
            set yn [expr {$ry * ($x*$sin_phi + $y*$cos_phi) + $dy}]
            lappend res $xn $yn
        }
        return $res
    }

    # Create a four-point spline that forms an arc along the unit circle
    proc Simplearc {phi2} {
        set x0 [expr {cos($phi2)}]
        set y0 [expr {-sin($phi2)}]
        set x3 $x0
        set y3 [expr {-$y0}]
        set x1 [expr {0.3333*(4.0-$x0)}]
        set y1 [expr {(1.0-$x0)*(3.0-$x0)/(3.0*$y0)}]
        set x2 $x1
        set y2 [expr {-$y1}]
        return [list $x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3]
    }

    method DrawArc {x0 y0 rx ry phi extend stroke filled style} {
        if {abs($extend) >= 360.0} {
            $self DrawOval $x0 $y0 $rx $ry $stroke $filled
            return
        }
        if {abs($extend) < 0.01} return
        $self EndTextObj

        set count 1
        while {abs($extend) > 90} {
            set count [expr {2*$count}]
            set extend [expr {0.5*$extend}]
        }
        set phi [expr {$phi/180.0*3.1416}]
        set extend [expr {$extend/180.0*3.1416}]
        set phi2 [expr {0.5*$extend}]
        set x [expr {$x0+$rx*cos($phi)}]
        set y [expr {$y0+$ry*sin($phi)}]
        $self Pdfoutcmd $x $y "m"
        set points [Simplearc $phi2]
        set phi [expr {$phi+$phi2}]
        for {set i 0} {$i < $count} {incr i} {
            foreach {x y x1 y1 x2 y2 x3 y3}                      [Transform $rx $ry $phi $x0 $y0 $points] break
            set phi [expr {$phi+$extend}]
            $self Pdfoutcmd $x1 $y1 $x2 $y2 $x3 $y3 "c"
        }
        switch $style {
            "arc" {
                set filled 0
            }
            "pieslice" {
                # Add the line to the center
                $self Pdfoutcmd $x0 $y0 "l"
                # Close the path
                $self Pdfoutcmd "h"
            }
            "chord" {
                # Close the path
                $self Pdfoutcmd "h"
            }
        }
        if {$filled && $stroke} {
            $self Pdfoutcmd "B"
        } elseif {$filled && !$stroke} {
            $self Pdfoutcmd "f"
        } else {
            $self Pdfoutcmd " S"
        }
    }

    # Draw an arc
    method arc {x0 y0 rx ry phi extend args} {
        if {!$pdf(inPage)} { $self startPage }
        set filled 0
        set stroke 1
        set style arc

        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled $value
                }
                "-stroke" {
                    set stroke $value
                }
                "-style" {
                    set style $value
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }

        $self Trans $x0 $y0 x0 y0
        set rx [$self GetPoints $rx]
        set ry [$self GetPoints $ry]

        $self DrawArc $x0 $y0 $rx $ry $phi $extend $stroke $filled $style
    }

    method arrow {x1 y1 x2 y2 sz {angle 20}} {
        if {!$pdf(inPage)} { $self startPage }
        $self Trans $x1 $y1 x1 y1
        $self Trans $x2 $y2 x2 y2
        set sz [$self GetPoints $sz]

        $self DrawLine $x1 $y1 $x2 $y2
        set rad [expr {$angle*3.1415926/180.0}]
        set ang [expr {atan2(($y1-$y2), ($x1-$x2))}]
        $self DrawLine $x2 $y2 [expr {$x2+$sz*cos($ang+$rad)}] [expr {$y2+$sz*sin($ang+$rad)}]
        $self DrawLine $x2 $y2 [expr {$x2+$sz*cos($ang-$rad)}] [expr {$y2+$sz*sin($ang-$rad)}]
    }

    method setBgColor {args} {
        set pdf(bgColor) [GetColor $args]
    }

    method setFillColor {args} {
        if {!$pdf(inPage)} { $self startPage }
        set pdf(fillColor) [GetColor $args]
        foreach {red green blue} $pdf(fillColor) break
        $self Pdfoutcmd $red $green $blue "rg"
    }

    method setStrokeColor {args} {
        if {!$pdf(inPage)} { $self startPage }
        set pdf(strokeColor) [GetColor $args]
        foreach {red green blue} $pdf(strokeColor) break
        $self Pdfoutcmd $red $green $blue "RG"
    }

    # Draw a rectangle, internal version
    method DrawRect {x y w h stroke filled} {
        $self Pdfoutcmd $x $y $w $h "re"
        if {$filled && $stroke} {
            $self Pdfoutcmd "B"
        } elseif {$filled && !$stroke} {
            $self Pdfoutcmd "f"
        } else {
            $self Pdfoutcmd "S"
        }
    }

    # Draw a rectangle
    method rectangle {x y w h args} {
        $self EndTextObj

        set filled 0
        set stroke 1
        foreach {arg value} $args {
            switch -- $arg {
                "-filled" {
                    set filled $value
                }
                "-stroke" {
                    set stroke $value
                }
                default {
                    return -code error "unknown option $arg"
                }
            }
        }
        $self Trans $x $y x y
        $self TransR $w $h w h

        $self DrawRect $x $y $w $h $stroke $filled
    }

    #######################################################################
    # Image Handling
    #######################################################################

    # Add an image to the document
    method addImage {filename args} {
        if {!$pdf(inPage)} { $self startPage }
        set id ""
        set type ""
        foreach {arg val} $args {
            switch -- $arg {
                -id {
                    set id $val
                }
                -type {
                    set type $val
                }
            }
        }

        if {$type eq ""} {
            switch -glob $filename {
                *.png {
                    set type png
                }
                *.jpg - *.jpeg {
                    set type jpg
                }
                default {
                    return -code error "Unknown image type $filename"
                }
            }
        }
        switch $type {
            png {
                set id [$self AddPng $filename $id]
            }
            jpg - jpeg {
                set id [$self addJpeg $filename $id]
            }
            default {
                return -code error "Unknown image type $type"
            }
        }
        return $id
    }

    # Deprecated jpeg adder, use addImage
    method addJpeg {filename id} {
        if {!$pdf(inPage)} { $self startPage }
        variable images

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }

        fconfigure $if -translation binary
        set img [read $if]
        close $if
        binary scan $img "H4" h
        if {$h != "ffd8"} {
            return -code error "file $filename does not contain JPEG data."
        }
        set pos 2
        set img_length [string length $img]
        while {$pos < $img_length} {
            set endpos [expr {$pos+4}]
            binary scan [string range $img $pos $endpos] "H4S" h length
            set length [expr {$length & 0xffff}]
            if {$h == "ffc0"} {
                incr pos 4
                set endpos [expr {$pos+6}]
                binary scan [string range $img $pos $endpos] "cSS" depth height width
                set height [expr {$height & 0xffff}]
                set width [expr {$width & 0xffff}]
                set imgOK true
                break
            } else {
                incr pos 2
                incr pos $length
            }
        }
        if {!$imgOK} {
            return -code error "something is wrong with jpeg data in file $filename"
        }
        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"
        append xobject "/ColorSpace /DeviceRGB\n"
        append xobject "/BitsPerComponent $depth\n"
        append xobject "/Filter /DCTDecode\n"
        append xobject "/Length $img_length >>\n"
        append xobject "stream\n"
        append xobject $img
        append xobject "\nendstream"

        set oid [$self AddObject $xobject]

        if {$id eq ""} {
            set id image$oid
        }
        set images($id) [list $width $height $oid]
        return $id
    }

    # PNG support
    #
    # This implementation uses tricks in PDF to avoid unpacking the
    # compressed data stream.  Currently this means that interlaced
    # images are not supported.
    # Decompressing (using zlib) would be feasible I guess, but the
    # de-filtering and de-interlacing steps would be rather costly.
    # Anyone needing such png images can always load them themselves
    # and provide them as raw images.

    method AddPng {filename id} {
        variable images

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }

        fconfigure $if -translation binary
        if {[read $if 8] != "\x89PNG\r\n\x1a\n"} {
            close $if
            return -code error "file does not contain PNG data"
        }
        set img [read $if]
        close $if

        set pos 0
        set img_length [string length $img]
        set img_data ""
        set palette ""
        while {$pos < $img_length} {
            # Scan one chunk
            binary scan $img "@${pos}Ia4" length type
            incr pos 8
            set data [string range $img $pos [expr {$pos + $length - 1}]]
            incr pos $length
            binary scan $img "@${pos}I" crc
            incr pos 4

            switch $type {
                "IHDR" {
                    set imgOK 1
                    binary scan $data IIccccc width height depth color                              compression filter interlace
                }
                "PLTE" {
                    set palette $data
                }
                "IDAT" {
                    append img_data $data
                }
            }
        }

        if {!$imgOK} {
            return -code error "something is wrong with PNG data in file $filename"
        }
        if {[string length $img_data] == 0} {
            return -code error "PNG file does not contain any IDAT chunks"
        }
        if {$compression != 0} {
            return -code error "PNG file is of an unsupported compression type"
        }
        if {$filter != 0} {
            return -code error "PNG file is of an unsupported filter type"
        }
        if {$interlace != 0} {
            # Would need to unpack and repack to do interlaced
            return -code error "Interlaced PNG is not supported"
        }

        if {$palette ne ""} {
            # Transform the palette into a PDF Indexed color space
            binary scan $palette H* PaletteHex
            set PaletteLen [expr {[string length $palette] / 3 - 1}]
            set paletteX "\[ /Indexed /DeviceRGB "
            append paletteX $PaletteLen " < "
            append paletteX $PaletteHex
            append paletteX " > \]"
        }

        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"

        if {$depth > 8} {
            $self RequireVersion 1.5
        }

        switch $color {
            0 { # Grayscale
                append xobject "/ColorSpace /DeviceGray\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            2 { # RGB
                append xobject "/ColorSpace /DeviceRGB\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 3 /BitsPerComponent $depth /Columns $width>>\n"
            }
            3 { # Palette
                append xobject "/ColorSpace $paletteX\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            4 { # Gray + alpha
                $self PngInitGrayAlpha
                append xobject "/ColorSpace $pdf(png_ga) 0 R\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 2 /BitsPerComponent $depth /Columns $width>>\n"
            }
            6 { # RGBA
                $self PngInitRgba
                append xobject "/ColorSpace $pdf(png_rgba) 0 R\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 4 /BitsPerComponent $depth /Columns $width>>\n"
            }
        }

        append xobject "/Length [string length $img_data] >>\n"
        append xobject "stream\n"
        append xobject $img_data
        append xobject "\nendstream"

        set oid [$self AddObject $xobject]

        if {$id eq ""} {
            set id image$oid
        }
        set images($id) [list $width $height $oid]
        return $id
    }

    # Create the Color Space needed to display RGBA as RGB
    method PngInitRgba {} {
        if {[info exists pdf(png_rgba)]} return
        set    body "<< /FunctionType 4\n"
        append body {/Domain [ 0.0  1.0  0.0  1.0 0.0  1.0 0.0  1.0 ]} \n
        append body {/Range [ 0.0  1.0 0.0  1.0 0.0  1.0 ]} \n
        append body {/Length 5} \n
        append body {>>} \n
        append body {stream} \n
        append body {{pop}} \n
        append body {endstream}
        set oid [$self AddObject $body]

        set body    "\[ /DeviceN\n"
        append body "   \[ /Red /Green /Blue /Alpha \]\n"
        append body "    /DeviceRGB\n"
        append body "    $oid 0 R   % Tint transformation function\n"
        append body "\]"
        set pdf(png_rgba) [$self AddObject $body]
    }

    # Create the Color Space needed to display Gray+Alpha as Gray
    method PngInitGrayAlpha {} {
        if {[info exists pdf(png_ga)]} return
        set    body "<< /FunctionType 4\n"
        append body {/Domain [ 0.0  1.0  0.0  1.0 ]} \n
        append body {/Range [ 0.0  1.0 ]} \n
        append body {/Length 5} \n
        append body {>>} \n
        append body {stream} \n
        append body {{pop}} \n
        append body {endstream}
        set oid [$self AddObject $body]

        set body    "\[ /DeviceN\n"
        append body "   \[ /_Gray_ /_Alpha_ \]\n"
        append body "    /DeviceGray\n"
        append body "    $oid 0 R   % Tint transformation function\n"
        append body "\]"
        set pdf(png_ga) [$self AddObject $body]
    }

    # Incomplete gif experiment...
    method AddGif {filename id} {
        variable images

        set imgOK false
        if {[catch {open $filename "r"} if]} {
            return -code error "Could not open file $filename"
        }

        fconfigure $if -translation binary
        set sign [read $if 6]
        if {![string match "GIF*" $sign]} {
            close $if
            return -code error "file does not contain GIF data"
        }
        set img [read $if]
        close $if

        set pos 0
        set img_length [string length $img]
        set img_data ""
        set palette ""

        # Read the screen descriptor
        binary scan $img "ssccc" scrWidth scrHeight cr bg dummy
        set pos 7
        set depth [expr {($cr & 7) + 1}]
        set colorMap [expr {($cr >> 7) & 1}]
        set colorRes [expr {($cr >> 4) & 7}]
        set nColor [expr {1 << $colorRes}]

        set gMap {}
        if {$colorMap} {
            for {set t 0} {$t < $nColor} {incr t} {
                binary scan $img "@${pos}ccc" red green blue
                incr pos 3
                lappend gMap $red $green $blue
            }
        }

        while {$pos < $img_length} {
            # Scan one chunk
            binary scan $img "@${pos}Ia4" length type
            incr pos 8
            set data [string range $img $pos [expr {$pos + $length - 1}]]
            incr pos $length
            binary scan $img "@${pos}I" crc
            incr pos 4

            switch $type {
                "IHDR" {
                    set imgOK 1
                    binary scan $data IIccccc width height depth color                              compression filter interlace
                }
                "PLTE" {
                    set palette $data
                }
                "IDAT" {
                    append img_data $data
                }
            }
        }

        if {!$imgOK} {
            return -code error "something is wrong with PNG data in file $filename"
        }
        if {[string length $img_data] == 0} {
            return -code error "PNG file does not contain any IDAT chunks"
        }
        if {$compression != 0} {
            return -code error "PNG file is of an unsupported compression type"
        }
        if {$filter != 0} {
            return -code error "PNG file is of an unsupported filter type"
        }
        if {$interlace != 0} {
            # Would need to unpack and repack to do interlaced
            return -code error "Interlaced PNG is not supported"
        }

        if {$palette ne ""} {
            # Transform the palette into a PDF Indexed color space
            binary scan $palette H* PaletteHex
            set PaletteLen [expr {[string length $palette] / 3 - 1}]
            set paletteX "\[ /Indexed /DeviceRGB "
            append paletteX $PaletteLen " < "
            append paletteX $PaletteHex
            append paletteX " > \]"
        }

        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"

        if {$depth > 8} {
            $self RequireVersion 1.5
        }

        switch $color {
            0 { # Grayscale
                append xobject "/ColorSpace /DeviceGray\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            2 { # RGB
                append xobject "/ColorSpace /DeviceRGB\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 3 /BitsPerComponent $depth /Columns $width>>\n"
            }
            3 { # Palette
                append xobject "/ColorSpace $paletteX\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent $depth /Columns $width>>\n"
            }
            4 { # Gray + alpha
                $self PngInitGrayAlpha
                append xobject "/ColorSpace $pdf(png_ga) 0 R\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 2 /BitsPerComponent $depth /Columns $width>>\n"
            }
            6 { # RGBA
                $self PngInitRgba
                append xobject "/ColorSpace $pdf(png_rgba) 0 R\n"
                append xobject "/BitsPerComponent $depth\n"
                append xobject "/Filter /FlateDecode\n"
                append xobject "/DecodeParms << /Predictor 15 /Colors 4 /BitsPerComponent $depth /Columns $width>>\n"
            }
        }

        append xobject "/Length [string length $img_data] >>\n"
        append xobject "stream\n"
        append xobject $img_data
        append xobject "\nendstream"

        set oid [$self AddObject $xobject]

        if {$id eq ""} {
            set id image$oid
        }
        set images($id) [list $width $height $oid]
        return $id
    }

    # Place an image at the page
    method putImage {id x y args} {
        $self EndTextObj
        variable images
        foreach {width height oid} $images($id) {break}

        $self Trans $x $y x y
        set w $width
        set h $height
        set wfix 0
        set hfix 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width"  {set w [$self GetPoints $value]; set wfix 1}
                "-height" {set h [$self GetPoints $value]; set hfix 1}
            }
        }
        if {$wfix && !$hfix} {
            set h [expr {$height*$w/$width}]
        }
        if {$hfix && !$wfix} {
            set w [expr {$width*$h/$height}]
        }

        if {$pdf(orient)} {
            set y [expr {$y-$h}]
        }
        $self Pdfoutcmd "q"
        $self Pdfoutcmd $w 0 0 $h $x $y "cm"
        $self Pdfout "/$id Do\nQ\n"
    }

    # Add a raw image to the document, to be placed later
    method addRawImage {img_data args} {
        if {!$pdf(inPage)} { $self startPage }
        variable images
        # Determine the width and height of the image, which is
        # a list of lists(rows).
        set width [llength [lindex $img_data 0]]
        set height [llength $img_data]

        set id ""
        foreach {arg value} $args {
            switch -- $arg {
                "-id"     {set id $value}
            }
        }

        set    xobject "<<\n/Type /XObject\n"
        append xobject "/Subtype /Image\n"
        append xobject "/Width $width\n/Height $height\n"
        append xobject "/ColorSpace /DeviceRGB\n"
        append xobject "/BitsPerComponent 8>>\n"
        append xobject "stream\n"

        # Iterate on each row of the image data.
        set img ""
        foreach rawRow $img_data {
            # Remove spaces and # characters
            regsub -all -- {((\#)|( ))} $rawRow {} row
            # Convert data to binary format and
            # add to data stream.
            append img [binary format H* $row]
        }

        append xobject $img
        append xobject "\nendstream"

        set oid [$self AddObject $xobject]

        if {$id eq ""} {
            set id image$oid
        }
        set images($id) [list $width $height $oid]
        return $id
    }

    # Place a raw image at the page
    method putRawImage {img_data x y args} {
        $self EndTextObj
        # Determine the width and height of the image, which is
        # a list of lists(rows).
        set width [llength [lindex $img_data 0]]
        set height [llength $img_data]

        $self Trans $x $y x y
        set w $width
        set h $height
        set wfix 0
        set hfix 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width"  {set w [$self GetPoints $value]; set wfix 1}
                "-height" {set h [$self GetPoints $value]; set hfix 1}
            }
        }
        if {$wfix && !$hfix} {
            set h [expr {$height*$w/$width}]
        }
        if {$hfix && !$wfix} {
            set w [expr {$width*$h/$height}]
        }

        if {$pdf(orient)} {
            set y [expr {$y-$h}]
        }
        $self Pdfoutcmd "q"
        $self Pdfoutcmd $w 0 0 $h $x $y "cm"
        $self Pdfoutcmd "BI"
        $self Pdfoutn   "/W [Nf $width]"
        $self Pdfoutn   "/H [Nf $height]"
        $self Pdfoutn   "/CS /RGB"
        $self Pdfoutn   "/BPC 8"
        $self Pdfoutcmd "ID"

        # Iterate on each row of the image data.
        foreach rawRow $img_data {
            # Remove spaces and # characters
            regsub -all -- {((\#)|( ))} $rawRow {} row
            # Convert data to binary format and
            # add to data stream.
            $self Pdfout [binary format H* $row]
        }

        $self Pdfout    \n
        $self Pdfoutcmd "EI"
        $self Pdfoutcmd "Q"
    }

    # Add a bitmap to the document, as a pattern
    method AddBitmap {bitmap args} {
        variable bitmaps
        variable patterns

        set id ""
        set pattern ""
        foreach {arg value} $args {
            switch -- $arg {
                "-id"      {set id $value}
                "-pattern" {set pattern $value}
            }
        }

        # Load the bitmap file
        if {[string index $bitmap 0] eq "@"} {
            set filename [string range $bitmap 1 end]
        } else {
            # Internal bitmap
            set filename [file join $pdf4tcl::dir "bitmaps" ${bitmap}.xbm]
        }
        if {![file exists $filename]} {
            return -code error "No such bitmap $bitmap"
        }
        set ch [open $filename "r"]
        set bitmapdata [read $ch]
        close $ch
        if {![regexp {_width (\d+)} $bitmapdata -> width]} {
            return -code error "Not a bitmap $bitmap"
        }
        if {![regexp {_height (\d+)} $bitmapdata -> height]} {
            return -code error "Not a bitmap $bitmap"
        }
        if {![regexp {_bits\s*\[\]\s*=\s*\{(.*)\}} $bitmapdata -> rawdata]} {
            return -code error "Not a bitmap $bitmap"
        }
        set bytes [regexp -all -inline {0x[a-fA-F0-9]{2}} $rawdata]
        set bytesPerLine [expr {[llength $bytes] / $height}]

        set bits ""
        foreach byte $bytes {
            # Reverse bit order
            for {set t 0} {$t < 8} {incr t} {
                append bits [expr {1 & $byte}]
                set byte [expr {$byte >> 1}]
            }
        }
        set bitstream [binary format B* $bits]

        if {$pattern eq ""} {
            # The Image Mask Object can be used as transparency Mask
            # for something else, e.g. when drawing the bitmap itself
            # with transparent background.

            set    xobject "<<\n/Type /XObject\n"
            append xobject "/Subtype /Image\n"
            append xobject "/Width $width\n/Height $height\n"
            append xobject {/ImageMask true /Decode [ 1 0 ]} \n
            append xobject "/BitsPerComponent 1\n"
            append xobject "/Length [string length $bitstream]\n"
            append xobject ">>\nstream\n"
            append xobject $bitstream
            append xobject "\nendstream"

            set imoid [$self AddObject $xobject]
            if {$id eq ""} {
                set id bitmap$imoid
            }
            set bitmaps($id) [list $width $height $imoid $bitstream]
            return $id
        } else {
            # Inline image within the Pattern Object
            set    stream "q\n"
            append stream "$width 0 0 $height 0 0 " "cm" \n
            append stream "BI\n"
            append stream "/W [Nf $width]\n"
            append stream "/H [Nf $height]\n"
            append stream {/IM true /Decode [ 1 0 ]} \n
            append stream "/BPC 1\n"
            append stream "ID\n"
            append stream $bitstream
            append stream ">\nEI\nQ"

            # The Pattern Object can be used as a stipple Mask with the Cs1
            # Colorspace.
            
            if {[llength $pattern] == 4} {
                foreach {xscale yscale xoffset yoffset} $pattern break
            } else {
                set xscale 1
                set yscale 1
                set xoffset 0
                set yoffset 0
            }

            set xobject "<<\n/Type /Pattern\n"
            append xobject "/PatternType 1\n"
            append xobject "/PaintType 2\n"
            append xobject "/TilingType 1\n"
            append xobject "/BBox \[ 0 0 $width $height \]\n"
            append xobject "/XStep $width\n"
            append xobject "/YStep $height\n"
            append xobject "/Matrix \[ $xscale 0 0 $yscale $xoffset $yoffset \] \n"
            append xobject "/Resources <<\n"
            append xobject ">>\n"
            append xobject "/Length [string length $stream]\n"
            append xobject ">>\n"
            append xobject "stream\n"
            append xobject $stream
            append xobject "\nendstream"

            set oid [$self AddObject $xobject]

            if {$id eq ""} {
                set id pattern$oid
            }
            set patterns($id) [list $width $height $oid]
            return $id
        }
    }

    #######################################################################
    # Canvas Handling
    #######################################################################

    method canvas {path args} {
        $self EndTextObj

        set sticky "nw"
        $self Trans 0 0 x y
        set width ""
        set height ""
        set bbox [$path bbox all]
        set bg 0
        foreach {arg value} $args {
            switch -- $arg {
                "-width"  {set width [$self GetPoints $value]}
                "-height" {set height [$self GetPoints $value]}
                "-sticky" {set sticky $value}
                "-y"      {$self Trans 0 $value _ y}
                "-x"      {$self Trans $value 0 x _}
                "-bbox"   {set bbox $value}
                "-bg"     {set bg $value}
                default {
                    return -code error                              "unknown option $arg"
                }
            }
        }
        if {$bbox eq ""} {
            # Nothing to display
            return
        }
        if {$width eq ""} {
            set width [expr {$pdf(width) -                      $pdf(marginright) - $x}]
        }
        if {$height eq ""} {
            if {$pdf(orient)} {
                set height [expr {$y - $pdf(marginbottom)}]
            } else {
                set height [expr {$pdf(height) - $pdf(margintop) - $y}]
            }
        }
        if {[llength $bbox] != 4} {
            return -code error "-bbox must be a four element list"
        }
        foreach {bbx1 bby1 bbx2 bby2} $bbox break
        set bbw [expr {$bbx2 - $bbx1}]
        set bbh [expr {$bby2 - $bby1}]

        set stickyw [string match "*w*" $sticky]
        set stickye [string match "*e*" $sticky]
        set stickyn [string match "*n*" $sticky]
        set stickys [string match "*s*" $sticky]
        set fillx [expr {$stickyw && $stickye}]
        set filly [expr {$stickyn && $stickys}]

        # Now calculate offset and scale between canvas coords
        # and pdf coords.

        set xscale  [expr {$width / $bbw}]
        set yscale  [expr {$height / $bbh}]

        if {$xscale > $yscale && !$fillx} {
            set xscale $yscale
        }
        if {$yscale > $xscale && !$filly} {
            set yscale $xscale
        }

        set xoffset [expr {$x - $bbx1 * $xscale}]
        if {!$fillx && !$stickyw} {
            # Move right
            set xoffset [expr {$xoffset + ($width - $bbw * $xscale)}]
        }

        if {$pdf(orient)} {
            set yoffset $y
        } else {
            set yoffset [expr {$y + $height}]
        }
        set yoffset [expr {$yoffset + $bby1 * $yscale}]
        if {!$filly && !$stickyn} {
            # Move down
            set yoffset [expr {$yoffset - ($height - $bbh * $yscale)}]
        }

        # Canvas coordinate system starts in upper corner
        # Thus we need to flip the y axis
        set yscale [expr {-$yscale}]

        # Set up clean graphics modes

        $self Pdfoutcmd "q"
        $self Pdfoutcmd 1.0 "w"
        $self Pdfout "\[\] 0 d\n"
        $self Pdfoutcmd 0 0 0 "rg"
        $self Pdfoutcmd 0 0 0 "RG"
        $self Pdfoutcmd 0 "J" ;# Butt cap style
        $self Pdfoutcmd 0 "j" ;# Miter join style
        # Miter limit; Tk switches from miter to bevel at 11 degrees
        $self Pdfoutcmd [expr {1.0/sin(11.0/180.0*3.14159265/2.0)}] "M"
        # Store scale. Used to get the correct size of stipple patterns.
        set pdf(canvasscale) [list [Nf $xscale] [Nf [expr {-$yscale}]]                  [Nf $xoffset] [Nf $yoffset]]
        
        # Use better resolution for the scale since that can be small numbers
        $self Pdfoutn [Nf $xscale 6] 0 0 [Nf $yscale 6]                  [Nf $xoffset] [Nf $yoffset] "cm"

        # Clip region
        $self Pdfoutcmd $bbx1 $bby1 "m"
        $self Pdfoutcmd $bbx1 $bby2 "l"
        $self Pdfoutcmd $bbx2 $bby2 "l"
        $self Pdfoutcmd $bbx2 $bby1 "l"
        #$self Pdfoutcmd $bbx1 $bby1 $bbw $bbh "re"
        $self Pdfoutcmd "W"
        if {$bg} {
            # Draw the region in background color if requested
            foreach {red green blue} [GetColor [$path cget -background]] break
            $self Pdfoutcmd $red $green $blue "rg"
            $self Pdfoutcmd "f"
            $self Pdfoutcmd 0 0 0 "rg"
        } else {
            $self Pdfoutcmd "n"
        }

        #set enclosed [$path find enclosed $bbx1 $bby1 $bbx2 $bby2]
        set overlapping [$path find overlapping $bbx1 $bby1 $bbx2 $bby2]
        foreach id $overlapping {
            set coords [$path coords $id]
            CanvasGetOpts $path $id opts
            if {[info exists opts(-state)] && $opts(-state) eq "hidden"} {
                continue
            }
            # Save graphics state for each item
            $self Pdfoutcmd "q"

            $self CanvasDoItem $path $id $coords opts

            # Restore graphics state after the item
            $self Pdfoutcmd "Q"
        }
        # Restore graphics state after the canvas
        $self Pdfoutcmd "Q"
    }

    # Handle one canvas item
    method CanvasDoItem {path id coords optsName} {
        upvar 1 $optsName opts
        variable images
        variable bitmaps

        # Not implemented: line/polygon -splinesteps
        # Not implemented: stipple offset
        # Limited: Stipple scale and offset does not match screen display
        # Limited: window item needs Img, and needs to be mapped

        switch [$path type $id] {
            rectangle {
                foreach {x1 y1 x2 y2} $coords break
                set w [expr {$x2 - $x1}]
                set h [expr {$y2 - $y1}]

                $self CanvasStdOpts opts
                set stroke [expr {$opts(-outline) ne ""}]
                set filled [expr {$opts(-fill) ne ""}]

                $self DrawRect $x1 $y1 $w $h $stroke $filled
            }
            line {
                # For a line, -fill means the stroke colour
                set opts(-outline)        $opts(-fill)
                set opts(-outlinestipple) $opts(-stipple)
                set opts(-outlineoffset)  $opts(-offset)
                $self CanvasStdOpts opts

                set arrows {}
                if {$opts(-arrow) eq "first" || $opts(-arrow) eq "both"} {
                    lappend arrows [lindex $coords 2] [lindex $coords 3]                              [lindex $coords 0] [lindex $coords 1] 0
                }
                if {$opts(-arrow) eq "last" || $opts(-arrow) eq "both"} {
                    lappend arrows [lindex $coords end-3] [lindex $coords end-2]                              [lindex $coords end-1] [lindex $coords end] [expr {[llength $coords] - 2}]
                }
                if {[llength $arrows] > 0} {
                    foreach {shapeA shapeB shapeC} $opts(-arrowshape) break
                    # Adjust like Tk does
                    set shapeA [expr {$shapeA + 0.001}]
                    set shapeB [expr {$shapeB + 0.001}]
                    set shapeC [expr {$shapeC + $opts(-width) / 2.0 + 0.001}]

                    set fracHeight [expr {($opts(-width)/2.0)/$shapeC}]
                    set backup  [expr {$fracHeight * $shapeB +                              $shapeA * (1.0 - $fracHeight)/2.0}]
                    foreach {x1 y1 x2 y2 ix} $arrows {
                        set poly [list 0 0 0 0 0 0 0 0 0 0 0 0]
                        lset poly 0  $x2
                        lset poly 10 $x2
                        lset poly 1  $y2
                        lset poly 11 $y2
                        set dx [expr {$x2 - $x1}]
                        set dy [expr {$y2 - $y1}]
                        set length [expr {hypot($dx, $dy)}]
                        if {$length == 0} {
                            set sinTheta 0.0
                            set cosTheta 0.0
                        } else {
                            set sinTheta [expr {$dy / $length}]
                            set cosTheta [expr {$dx / $length}]
                        }
                        set  vertX  [expr {[lindex $poly 0] - $shapeA * $cosTheta}]
                        set  vertY  [expr {[lindex $poly 1] - $shapeA * $sinTheta}]
                        set  temp   [expr {                   $shapeC * $sinTheta}]
                        lset poly 2 [expr {[lindex $poly 0] - $shapeB * $cosTheta + $temp}]
                        lset poly 8 [expr {[lindex $poly 2] - 2 * $temp}]
                        set  temp   [expr {                   $shapeC * $cosTheta}]
                        lset poly 3 [expr {[lindex $poly 1] - $shapeB * $sinTheta - $temp}]
                        lset poly 9 [expr {[lindex $poly 3] + 2 * $temp}]
                        lset poly 4 [expr {[lindex $poly 2] * $fracHeight + $vertX * (1.0-$fracHeight)}]
                        lset poly 5 [expr {[lindex $poly 3] * $fracHeight + $vertY * (1.0-$fracHeight)}]
                        lset poly 6 [expr {[lindex $poly 8] * $fracHeight + $vertX * (1.0-$fracHeight)}]
                        lset poly 7 [expr {[lindex $poly 9] * $fracHeight + $vertY * (1.0-$fracHeight)}]

                        # Adjust line end to draw it under the arrow
                        lset coords $ix [expr {[lindex $coords $ix] - $backup * $cosTheta}]
                        incr ix
                        lset coords $ix [expr {[lindex $coords $ix] - $backup * $sinTheta}]

                        # Draw polygon
                        set cmd "m"
                        foreach {x y} $poly {
                            $self Pdfoutcmd $x $y $cmd
                            set cmd "l"
                        }
                        $self Pdfoutcmd "f"
                    }
                }

                # Draw lines
                if {([string is true -strict $opts(-smooth)] ||                          $opts(-smooth) eq "bezier") && [llength $coords] > 4} {
                    $self CanvasBezier $coords
                } elseif {$opts(-smooth) eq "raw"} {
                    $self CanvasRawCurve $coords
                } else {
                    set cmd "m"
                    foreach {x y} $coords {
                        $self Pdfoutcmd $x $y $cmd
                        set cmd "l"
                    }
                }
                $self Pdfoutcmd "S"
            }
            oval {
                foreach {x1 y1 x2 y2} $coords break
                set x  [expr {($x2 + $x1) / 2.0}]
                set y  [expr {($y2 + $y1) / 2.0}]
                set rx [expr {($x2 - $x1) / 2.0}]
                set ry [expr {($y2 - $y1) / 2.0}]

                $self CanvasStdOpts opts
                set stroke [expr {$opts(-outline) ne ""}]
                set filled [expr {$opts(-fill) ne ""}]

                $self DrawOval $x $y $rx $ry $stroke $filled
            }
            arc {
                foreach {x1 y1 x2 y2} $coords break
                set x  [expr {($x2 + $x1) / 2.0}]
                set y  [expr {($y2 + $y1) / 2.0}]
                set rx [expr {($x2 - $x1) / 2.0}]
                # Flip y-axis
                set ry [expr {-($y2 - $y1) / 2.0}]

                # Canvas draws arc with bevel style
                if {![info exists opts(-joinstyle)]} {
                    set opts(-joinstyle) bevel
                }
                $self CanvasStdOpts opts
                set stroke [expr {$opts(-outline) ne ""}]
                set filled [expr {$opts(-fill) ne ""}]

                set phi $opts(-start)
                set extend $opts(-extent)

                $self DrawArc $x $y $rx $ry $phi $extend $stroke $filled                          $opts(-style)
            }
            polygon {
                $self CanvasStdOpts opts
                set stroke [expr {$opts(-outline) ne ""}]
                set filled [expr {$opts(-fill) ne ""}]

                if {[string is true -strict $opts(-smooth)] ||                              $opts(-smooth) eq "bezier"} {
                    # Close the coordinates if necessary
                    if {[lindex $coords 0] != [lindex $coords end-1] ||                                  [lindex $coords 1] != [lindex $coords end]} {
                        lappend coords [lindex $coords 0] [lindex $coords 1]
                    }
                    $self CanvasBezier $coords
                } elseif {$opts(-smooth) eq "raw"} {
                    $self CanvasRawCurve $coords
                } else {
                    set cmd "m"
                    foreach {x y} $coords {
                        $self Pdfoutcmd $x $y $cmd
                        set cmd "l"
                    }
                }
                if {$filled && $stroke} {
                    $self Pdfoutcmd "b"
                } elseif {$filled && !$stroke} {
                    $self Pdfoutcmd "f"
                } else {
                    $self Pdfoutcmd "s"
                }
            }
            text {
                # Width is not a stroke option here
                array unset opts -width
                $self CanvasStdOpts opts

                set lines [CanvasGetWrappedText $path $id underline]
                foreach {x y} $coords break
                foreach {x1 y1 x2 y2} [$path bbox $id] break

                $self CanvasSetFont $opts(-font)
                set fontsize $pdf(font_size)
                # Next, figure out if the text fits within the bbox
                # with the current font, or it needs to be scaled.
                set widest 0.0
                foreach line $lines {
                    set width [$self getStringWidth $line 1]
                    if {$width > $widest} {
                        set widest $width
                    }
                }
                set xscale [expr {$widest / ($x2 - $x1)}]
                set yscale [expr {([llength $lines] * $fontsize) /                          ($y2 - $y1)}]
                # Scale down if the font is too big
                if {$xscale > 1.001} {
                    $self setFont [expr {$fontsize / $xscale}] "" 1
                    set fontsize $pdf(font_size)
                    set widest [expr {$widest / $xscale}]
                }

                # Now we have selected an appropriate font and size.

                # Move x/y to point nw/n/ne depending on anchor
                # and justification
                set width $widest
                set height [expr {$fontsize * [llength $lines]}]
                if {[string match "s*" $opts(-anchor)]} {
                    set y [expr {$y - $height}]
                } elseif {![string match "n*" $opts(-anchor)]} {
                    set y [expr {$y - ($height / 2.0)}]
                }
                if {[string match "*w" $opts(-anchor)]} {
                    set xanchor 0
                } elseif {[string match "*e" $opts(-anchor)]} {
                    set xanchor 2
                } else {
                    set xanchor 1
                }
                set xjustify [lsearch {left center right} $opts(-justify)]
                set x [expr {$x + ($xjustify - $xanchor) * $width / 2.0}]

                # Displace y to base line of font
                set bboxy [$self getFontMetric bboxy 1]
                set y [expr {$y + $bboxy + $fontsize}]
                set lineNo 0
                set ulcoords {}
                foreach line $lines {
                    set width [$self getStringWidth $line 1]
                    set x0 [expr {$x - $xjustify * $width / 2.0}]

                    # Since we have put the coordinate system  upside
                    # down to follow canvas coordinates we need a
                    # negative y scale here to get the text correct.

                    $self Pdfoutcmd 1 0 0 -1 $x0 $y "Tm"
                    $self Pdfout "([CleanText $line]) Tj\n"

                    if {$underline != -1} {
                        if {[lindex $underline 0] eq $lineNo} {
                            set index [lindex $underline 1]
                            set ulx [$self getStringWidth [string range $line                                                 0 [expr {$index - 1}]] 1]
                            set ulw [$self getStringWidth [string index $line $index] 1]
                            lappend ulcoords [expr {$x0 + $ulx}]                                      [expr {$y - $bboxy}] $ulw
                        }
                    }
                    incr lineNo
                    set y [expr {$y + $fontsize}]
                }
                $self EndTextObj

                # Draw any underline
                foreach {x y w} $ulcoords {
                    $self Pdfoutcmd $x $y "m"
                    $self Pdfoutcmd [expr {$x + $w}] $y "l"
                    $self Pdfoutcmd "S"
                }
            }
            bitmap {
                set bitmap $opts(-bitmap)
                if {$bitmap eq ""} {
                    return
                }
                set id bitmap_canvas_[file rootname [file tail $bitmap]]
                if {![info exists bitmaps($id)]} {
                    $self AddBitmap $bitmap -id $id
                }
                foreach {width height imoid stream} $bitmaps($id) break
                foreach {x1 y1} $coords break
                # Since the canvas coordinate system is upside
                # down we must flip back to get the image right.
                # We do this by adjusting y and y scale.
                switch $opts(-anchor) {
                    nw { set dx 0.0 ; set dy 1.0 }
                    n  { set dx 0.5 ; set dy 1.0 }
                    ne { set dx 1.0 ; set dy 1.0 }
                    e  { set dx 1.0 ; set dy 0.5 }
                    se { set dx 1.0 ; set dy 0.0 }
                    s  { set dx 0.5 ; set dy 0.0 }
                    sw { set dx 0.0 ; set dy 0.0 }
                    w  { set dx 0.0 ; set dy 0.5 }
                    default { set dx 0.5 ; set dy 0.5 }
                }
                set x [expr {$x1 - $width  * $dx}]
                set y [expr {$y1 + $height * $dy}]

                set bg $opts(-background)
                if {$bg eq ""} {
                    # Dummy background to see if masking fails
                    set bg $opts(-foreground)
                }
                # Build a two-color palette
                set colors [concat [GetColor $bg] [GetColor $opts(-foreground)]]
                set PaletteHex ""
                foreach color $colors {
                    append PaletteHex [format %02x                              [expr {int(round($color * 255.0))}]]
                }
                set paletteX "\[ /Indexed /DeviceRGB "
                append paletteX "1 < "
                append paletteX $PaletteHex
                append paletteX " > \]"

                # An image object for this bitmap+color
                set    xobject "<<\n/Type /XObject\n"
                append xobject "/Subtype /Image\n"
                append xobject "/Width $width\n/Height $height\n"
                append xobject "/ColorSpace $paletteX\n"
                append xobject "/BitsPerComponent 1\n"
                append xobject "/Length [string length $stream]\n"
                if {$opts(-background) eq ""} {
                    append xobject "/Mask $imoid 0 R\n"
                }
                append xobject ">>\n"
                append xobject "stream\n"
                append xobject $stream
                append xobject "\nendstream"

                set newoid [$self AddObject $xobject]
                set newid image$newoid
                set images($newid) [list $width $height $newoid]

                # Put the image on the page
                $self Pdfoutcmd $width 0 0 [expr {-$height}] $x $y "cm"
                $self Pdfout "/$newid Do\n"
            }
            image {
                set image $opts(-image)
                if {$image eq ""} {
                    return
                }
                set id image_canvas_$image
                if {![info exists images($id)]} {
                    $self addRawImage [$image data] -id $id
                }
                foreach {width height oid} $images($id) break
                foreach {x1 y1} $coords break
                # Since the canvas coordinate system is upside
                # down we must flip back to get the image right.
                # We do this by adjusting y and y scale.
                switch $opts(-anchor) {
                    nw { set dx 0.0 ; set dy 1.0 }
                    n  { set dx 0.5 ; set dy 1.0 }
                    ne { set dx 1.0 ; set dy 1.0 }
                    e  { set dx 1.0 ; set dy 0.5 }
                    se { set dx 1.0 ; set dy 0.0 }
                    s  { set dx 0.5 ; set dy 0.0 }
                    sw { set dx 0.0 ; set dy 0.0 }
                    w  { set dx 0.0 ; set dy 0.5 }
                    default { set dx 0.5 ; set dy 0.5 }
                }
                set x [expr {$x1 - $width  * $dx}]
                set y [expr {$y1 + $height * $dy}]

                $self Pdfoutcmd $width 0 0 [expr {-$height}] $x $y "cm"
                $self Pdfout "/$id Do\n"
            }
            window {
                catch {package require Img}
                if {[catch {image create photo -format window -data $opts(-window)} image]} {
                    set image ""
                }
                if {$image eq ""} {
                    # Get a size even if it is unmapped
                    foreach width [list [winfo width $opts(-window)]                                          $opts(-width)                                          [winfo reqwidth $opts(-window)]] {
                        if {$width > 1} break
                    }
                    foreach height [list [winfo height $opts(-window)]                                           $opts(-height)                                           [winfo reqheight $opts(-window)]] {
                        if {$height > 1} break
                    }
                } else {
                    set id [$self addRawImage [$image data]]

                    foreach {width height oid} $images($id) break
                }
                foreach {x1 y1} $coords break
                # Since the canvas coordinate system is upside
                # down we must flip back to get the image right.
                # We do this by adjusting y and y scale.
                switch $opts(-anchor) {
                    nw { set dx 0.0 ; set dy 1.0 }
                    n  { set dx 0.5 ; set dy 1.0 }
                    ne { set dx 1.0 ; set dy 1.0 }
                    e  { set dx 1.0 ; set dy 0.5 }
                    se { set dx 1.0 ; set dy 0.0 }
                    s  { set dx 0.5 ; set dy 0.0 }
                    sw { set dx 0.0 ; set dy 0.0 }
                    w  { set dx 0.0 ; set dy 0.5 }
                    default { set dx 0.5 ; set dy 0.5 }
                }
                set x [expr {$x1 - $width  * $dx}]
                set y [expr {$y1 + $height * $dy}]

                if {$image eq ""} {
                    # Draw a black box
                    $self Pdfoutcmd $x [expr {$y - $height}]                              $width $height "re"
                    $self Pdfoutcmd "f"
                } else {
                    $self Pdfoutcmd $width 0 0 [expr {-$height}] $x $y "cm"
                    $self Pdfout "/$id Do\n"
                }
            }
        }
    }

    method CanvasBezier {coords} {
        # Is it a closed curve?
        if {[lindex $coords 0] == [lindex $coords end-1] &&                      [lindex $coords 1] == [lindex $coords end]} {
            set closed 1

            set x0 [expr {0.5  * [lindex $coords end-3] + 0.5  *[lindex $coords 0]}]
            set y0 [expr {0.5  * [lindex $coords end-2] + 0.5  *[lindex $coords 1]}]
            set x1 [expr {0.167* [lindex $coords end-3] + 0.833*[lindex $coords 0]}]
            set y1 [expr {0.167* [lindex $coords end-2] + 0.833*[lindex $coords 1]}]
            set x2 [expr {0.833* [lindex $coords 0]     + 0.167*[lindex $coords 2]}]
            set y2 [expr {0.833* [lindex $coords 1]     + 0.167*[lindex $coords 3]}]
            set x3 [expr {0.5  * [lindex $coords 0]     + 0.5  *[lindex $coords 2]}]
            set y3 [expr {0.5  * [lindex $coords 1]     + 0.5  *[lindex $coords 3]}]
            $self Pdfoutcmd $x0 $y0 "m"
            $self Pdfoutcmd $x1 $y1 $x2 $y2 $x3 $y3 "c"
        } else {
            set closed 0
            set x3 [lindex $coords 0]
            set y3 [lindex $coords 1]
            $self Pdfoutcmd $x3 $y3 "m"
        }
        set len [llength $coords]
        for {set i 2} {$i < ($len - 2)} {incr i 2} {
            foreach {px1 py1 px2 py2} [lrange $coords $i [expr {$i + 3}]] break
            set x1 [expr {0.333*$x3 + 0.667*$px1}]
            set y1 [expr {0.333*$y3 + 0.667*$py1}]

            if {!$closed && $i == ($len - 4)} {
                # Last of an open curve
                set x3 $px2
                set y3 $py2
            } else {
                set x3 [expr {0.5 * $px1 + 0.5 * $px2}]
                set y3 [expr {0.5 * $py1 + 0.5 * $py2}]
            }
            set x2 [expr {0.333 * $x3 + 0.667 * $px1}]
            set y2 [expr {0.333 * $y3 + 0.667 * $py1}]
            $self Pdfoutcmd $x1 $y1 $x2 $y2 $x3 $y3 "c"
        }
    }

    method CanvasRawCurve {coords} {
        set x3 [lindex $coords 0]
        set y3 [lindex $coords 1]
        $self Pdfoutcmd $x3 $y3 "m"

        set len [llength $coords]
        # Is there a complete set of segements in the list?
        set add [expr {($len - 2) % 6}]
        if {$add != 0} {
            eval lappend coords [lrange $coords 0 [expr {$add - 1}]]
        }
        for {set i 0} {$i < ($len - 8)} {incr i 6} {
            foreach {px1 py1 px2 py2 px3 py3 px4 py4}                      [lrange $coords $i [expr {$i + 7}]] break
            if {$px1 == $px2 && $py1 == $py2 && $px3 == $px4 && $py3 == $py4} {
                # Straight line
                $self Pdfoutcmd $px4 $py4 "l"
            } else {
                $self Pdfoutcmd $px2 $py2 $px3 $py3 $px4 $py4 "c"
            }
        }
    }

    method CanvasGetBitmap {bitmap offset} {
        # The pattern is unique for the scale for this canvas
        foreach {xscale yscale xoffset yoffset} $pdf(canvasscale) break
        # Adapt to offset
        if {[regexp {^(\#?)(.*),(.*)$} $offset -> pre ox oy]} {
            set xoffset [expr {$xoffset + $ox * $xscale}]
            set yoffset [expr {$yoffset - $oy * $yscale}]
        } else {
            # Not supported yet
        }
        
        set scale [list $xscale $yscale $xoffset $yoffset]
        set tail [string map {. x} [join $scale _]]
        set id pattern_canvas_[file rootname [file tail $bitmap]]_$tail
        if {![info exists patterns($id)]} {
            $self AddBitmap $bitmap -id $id -pattern $scale
        }
        return $id
    }

    # Setup the graphics state from standard options
    method CanvasStdOpts {optsName} {
        upvar 1 $optsName opts
        variable patterns

        # Stipple for fill color
        set fillstippleid ""
        if {[info exists opts(-stipple)] && $opts(-stipple) ne ""} {
            set fillstippleid [$self CanvasGetBitmap $opts(-stipple)                      $opts(-offset)]
        }
        # Stipple for stroke color
        set strokestippleid ""
        if {[info exists opts(-outlinestipple)] &&                  $opts(-outlinestipple) ne ""} {
            # Outlineoffset is a 8.5 feature
            if {[info exists opts(-outlineoffset)]} {
                set offset $opts(-outlineoffset)
            } else {
                set offset $opts(-offset)
            }
            set strokestippleid [$self CanvasGetBitmap $opts(-outlinestipple)                      $offset]
        }
        # Outline controls stroke color
        if {[info exists opts(-outline)] && $opts(-outline) ne ""} {
            $self CanvasStrokeColor $opts(-outline) $strokestippleid
        }
        # Fill controls fill color
        if {[info exists opts(-fill)] && $opts(-fill) ne ""} {
            $self CanvasFillColor $opts(-fill) $fillstippleid
        }
        # Line width
        if {[info exists opts(-width)]} {
            $self Pdfoutcmd $opts(-width) "w"
        }
        # Dash pattern and offset
        if {[info exists opts(-dash)] && $opts(-dash) ne ""} {
            # FIXA: Support "..." and such
            $self Pdfout "\[$opts(-dash)\] $opts(-dashoffset) d\n"
        }
        # Cap style
        if {[info exists opts(-capstyle)] && $opts(-capstyle) ne "butt"} {
            switch $opts(-capstyle) {
                projecting {
                    $self Pdfoutcmd 2 "J"
                }
                round {
                    $self Pdfoutcmd 1 "J"
                }
            }
        }
        # Join style
        if {[info exists opts(-joinstyle)] && $opts(-joinstyle) ne "miter"} {
            switch $opts(-joinstyle) {
                bevel {
                    $self Pdfoutcmd 2 "j"
                }
                round {
                    $self Pdfoutcmd 1 "j"
                }
            }
        }
    }

    # Set the fill color from a Tk color
    method CanvasFillColor {color {bitmapid ""}} {
        foreach {red green blue} [GetColor $color] break
        if {$bitmapid eq ""} {
            $self Pdfoutcmd $red $green $blue "rg"
        } else {
            $self Pdfout "/Cs1 cs\n"
            #$self Pdfoutcmd $red $green $blue "scn"
            $self Pdfoutcmd $red $green $blue "/$bitmapid scn"
        }
    }

    # Set the stroke color from a Tk color
    method CanvasStrokeColor {color {bitmapid ""}} {
        foreach {red green blue} [GetColor $color] break
        if {$bitmapid eq ""} {
            $self Pdfoutcmd $red $green $blue "RG"
        } else {
            $self Pdfout "/Cs1 CS\n"
            $self Pdfoutcmd $red $green $blue "/$bitmapid SCN"
        }
    }

    # Helper to extract configuration from a canvas item
    proc CanvasGetOpts {path id arrName} {
        upvar 1 $arrName arr
        array unset arr
        foreach item [$path itemconfigure $id] {
            set arr([lindex $item 0]) [lindex $item 4]
        }
        if {![info exists arr(-state)]} {
            return
        }
        if {$arr(-state) eq "" || $arr(-state) eq "normal"} {
            return
        }
        # Translate options depending on state
        foreach item [array names arr] {
            if {[regexp -- "^-${state}(.*)\$" $item -> orig]} {
                if {[info exists arr(-$orig)]} {
                    set arr(-$orig) $arr($item)
                }
            }
        }
    }

    # Get the text from a text item, as a list of lines
    # This takes and line wrapping into account
    proc CanvasGetWrappedText {w item ulName} {
        upvar 1 $ulName underline
        set text  [$w itemcget $item -text]
        set width [$w itemcget $item -width]
        # Underline is a 8.5 feature
        if {[catch {$w itemcget $item -underline} underline]} {
            set underline -1
        }

        # Simple non-wrapping case. Only divide on newlines.
        if {$width == 0} {
            set lines [split $text \n]
            if {$underline != -1} {
                set isum 0
                set lineNo 0
                foreach line $lines {
                    set iend [expr {$isum + [string length $line]}]
                    if {$underline < $iend} {
                        set underline [list $lineNo [expr {$underline - $isum}]]
                        break
                    }
                    incr lineNo
                    set isum [expr {$iend + 1}]
                }
            }
            return $lines
        }

        # Run across the text's left side and look for all indexes
        # that start a line.

        foreach {x1 y1 x2 y2} [$w bbox $item] break
        set firsts {}
        for {set y $y1} {$y < $y2} {incr y} {
            lappend firsts [$w index $item @$x1,$y]
        }
        set firsts [lsort -integer -unique $firsts]

        # Extract each displayed line
        set prev 0
        set res {}
        foreach index $firsts {
            if {$prev != $index} {
                set line [string range $text $prev [expr {$index - 1}]]
                if {[string index $line end] eq "\n"} {
                    set line [string trimright $line \n]
                } else {
                    # If the line does not end with \n it is wrapped.
                    # Then spaces should be discarded
                    set line [string trimright $line]
                }
                lappend res $line
            }
            set prev $index
        }
        # The last chunk
        lappend res [string range $text $prev end]
        if {$underline != -1} {
            set lineNo -1
            set prev 0
            foreach index $firsts {
                if {$underline < $index} {
                    set underline [lindex $lineNo [expr {$underline - $prev}]]
                    break
                }
                set prev $index
                incr lineNo
            }
        }
        return $res
    }

    # Given a Tk font, figure out a reasonable font to use and set it
    # as current font.
    # In the future we could give more user options for controlling this.
    method CanvasSetFont {font} {
        array unset fontinfo
        array set fontinfo [font actual $font]
        array set fontinfo [font metrics $font]
        # Any fixed font maps to courier
        if {$fontinfo(-fixed)} {
            set fontinfo(-family) courier
        }
        set bold [expr {$fontinfo(-weight) eq "bold"}]
        set italic [expr {$fontinfo(-slant) eq "italic"}]

        switch -glob [string tolower $fontinfo(-family)] {
            *courier* - *fixed* {
                set family Courier
                if {$bold && $italic} {
                    append family -BoldOblique
                } elseif {$bold} {
                    append family -Bold
                } elseif {$italic} {
                    append family -BoldOblique
                }
            }
            *times* {
                if {$bold && $italic} {
                    set family Times-BoldItalic
                } elseif {$bold} {
                    set family Times-Bold
                } elseif {$italic} {
                    set family Times-Italic
                } else {
                    set family Times-Roman
                }
            }
            *helvetica* - *arial* - default {
                set family Helvetica
                if {$bold && $italic} {
                    append family -BoldOblique
                } elseif {$bold} {
                    append family -Bold
                } elseif {$italic} {
                    append family -BoldOblique
                }
            }
        }
        set fontsize $fontinfo(-linespace)
        $self BeginTextObj
        $self setFont $fontsize $family 1
    }

    #######################################################################
    # Helper fuctions
    #######################################################################

    # helper function: mask parentheses and backslash
    proc CleanText {in} {
        # Since the font is set up as WinAnsiEncoding, we get as close
        # as possible by using cp1252
        set in [encoding convertto cp1252 $in]
        return [string map {( \\( ) \\) \\ \\\\} $in]
    }

    # helper function: consume and return an object id
    method GetOid {{noxref 0}} {
        if {!$noxref} {
            $self StoreXref
        }
        set res $pdf(pdf_obj)
        incr pdf(pdf_obj)
        return $res
    }

    # helper function: return next object id (without incrementing)
    method NextOid {} {
        return $pdf(pdf_obj)
    }

    # helper function: set xref of (current) oid to current out_pos
    method StoreXref {{oid {}}} {
        if {$oid eq ""} {
            set oid $pdf(pdf_obj)
        }
        set pdf(xref,$oid) $pdf(out_pos)
    }

    # helper function for formatting floating point numbers
    proc Nf {n {deci 3}} {
        # Up to 3 decimals
        set num [format %.*f $deci $n]
        # Remove surplus decimals
        set num [string trimright [string trimright $num "0"] "."]
        # Small negative numbers might become -0
        if {$num eq "-0"} {
            set num "0"
        }
        return $num
    }
}

# vim: tw=0
# Tcl package index file, version 1.1
# This file is generated by the "pkg_mkIndex" command
# and sourced either when an application starts up or
# by a "package unknown" script.  It invokes the
# "package ifneeded" command to set up package-related
# information so that packages will be loaded automatically
# in response to "package require" commands.  When this
# script is sourced, the variable $dir must contain the
# full path name of this file's directory.

package ifneeded pdf4tcl 0.5 [list source [file join $dir pdf4tcl.tcl]]
package ifneeded pdf4tcl::metrics 0.3 [list source [file join $dir metrics.tcl]]
package ifneeded pdf4tcl::glyphnames 0.1 [list source [file join $dir glyphnames.tcl]]
    #source ./pdf4tcl05/glyphnames.tcl
    #source ./pdf4tcl05/metrics.tcl
    #source ./pdf4tcl05/pdf4tcl.tcl
    #source ./pdf4tcl05/pkgIndex.tcl

##-----------------------------------------------------------------------
##           graph.tcl -- the emu_graph widget for data plotting
##
##		       (c) Copyright 1996, 1997
##	     Speech Hearing and Language Research Centre,
##	       Macquarie University, Sydney, Australia.
##
##
##                       All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 	1. Redistributions of source code must retain the above
## 	copyright notice, this list of conditions and the following
## 	disclaimer.
##
## 	2. Redistributions in binary form must reproduce the above
## 	copyright notice, this list of conditions and the following
## 	disclaimer in the documentation and/or other materials provided
## 	with the distribution.
##
## 	3. Neither the name of Macquarie University nor the names of its
## 	contributors may be used to endorse or promote products derived
## 	from this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
## IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
## TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
## PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
## PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
## LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
## NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##-----------------------------------------------------------------------
## $Id: graph.tcl,v 1.3 1998/02/20 03:58:06 steve Exp $ 

# This file implements package emu_graph, which  draws a graph on 
#  a tk canvas
#
package provide emu_graph 2.0

namespace eval emu_graph {

## we only export emu_graph, everything else is accessed through the
## widget command
namespace export -clear emu_graph

## legal options 
set emu_graph(options) { 
    width height xref yref ticklen axistextoffset
    nticks_x nticks_y font autorange canvas xmin xmax ymin ymax
}

## default values for some options
set emu_graph(default) {      -width           300     -height          200     -xref            50     -yref            30     -ticklen         5     -axistextoffset  5     -nticks_x         5     -nticks_y         5     -font            fixed      -autorange       1 }

set emu_graph(dataoptions) {
    points lines colour coords mask maskthresh 	trackdata trackcolumn redraw labels
}

set emu_graph(datadefault) {
    -labels          {}      -points          0      -lines           1     -colour          red     -trackcolumn     0      -redraw          1
}

# here we will record all the graph names as they are made
set emu_graph(graphs) {}

## create a new emu_graph...
proc emu_graph args {
    variable emu_graph

    set graph [lindex $args 0]
    lappend emu_graph(graphs) $graph
    
    ## remove any existing config info under this name
    foreach key [array names emu_graph] {
        if [string match "$graph,*" $key] {
            unset emu_graph($key)
        }
    }

    ## prepend the default options to args, they can then be overridden if they
    ## appear later in the args
    set args [concat $graph 0 $emu_graph(default) [lrange $args 1 end]] 

    ## now parse the options
    set restargs [eval "internal_configure $args"]
    
    # shouldn't be any more args
    if { $restargs != {} } {
	error "Usage: emu_graph graph \[options\] ($restargs)"
    }
    set emu_graph($graph,datasets) {}
    
    # define the widget command
    namespace eval ::  	proc $graph args "\{namespace eval emu_graph \[concat emu_graph::invoke $graph  \$args\] \}"

}

proc invoke args {
    variable emu_graph;

    set graph [lindex $args 0]
    set method [lindex $args 1]

    if {[info procs $method] != {}} {
	eval [concat $method $graph [lrange $args 2 end]]
	} else { 
	    error "no method $method for emu_graph,\n options are [methods]"
        }
}


## find the names of all methods *, just giving the * bit
proc methods {} {
    return [info procs]
}


## implement the 'data' subcommand for graph objects
proc data args {

    variable emu_graph

    set graph [lindex $args 0]
    set tag [lindex $args 1]

    if {[llength $tag]>1 || [string match "-*" $tag]} {
	error "Usage: graph data tag \[options\]"
    }

    set args [concat $graph $tag $emu_graph(datadefault) [lrange $args 2 end]] 

    ## now parse the options
    set restargs [eval "internal_configure $args"]

    if { [llength $restargs] != 0 } {
	error "Usage: graph data tag \[options\]"
    }

    ## append tag only if not already exists, Mark Koennecke
    set mark_list $emu_graph($graph,datasets)
    if { [lsearch -exact $mark_list $tag] < 0 } {    
       set emu_graph($graph,datasets) [lappend emu_graph($graph,datasets) $tag]
    }
    set datalength 0 
    ## if we have data as coords then verify that each element is a pair 
    ## and remember the length for later
    if { [info exists emu_graph($graph,$tag,coords)] } {
	set ncoords [llength $emu_graph($graph,$tag,coords)]
	if { int($ncoords/2)*2 != $ncoords } {
	    set emu_graph($graph,$tag,coords) {}
	    error "bad data format in emu_graph $graph, data $tag\n -- length of coords must be even, it was $ncoords"
	}
	set datalength [expr $ncoords/2]
    }
    ## if we have data as trackdata, remember it's length
    if { [info exists emu_graph($graph,$tag,trackdata)] } {
	set datalength [$emu_graph($graph,$tag,trackdata) length]
    }

    # if there's a mask, chech that there's also a maskthresh and 
    # that the length of the mask is the same as the data
    if { $datalength != 0 && [info exists emu_graph($graph,$tag,mask)] } {
	if { ![info exists emu_graph($graph,$tag,maskthresh)] } {
	    error "No threshold supplied with masking vector in emu_graph, use -maskthresh N"
	}
	if { [llength $emu_graph($graph,$tag,mask)] != $datalength } {
	    error "Mask vector and coords have different lengths ([llength $emu_graph($graph,$tag,$mask)] and  $datalength)"
	}
    }
    if {$datalength != 0 && $emu_graph($graph,$tag,redraw)} {
	redraw $graph
    }
}

## make an image the backdrop of the graph, fit the graph axes around the
## image -- used for displaying a spectrogram image under formant plots
proc image {graph image xmin xmax ymin ymax} {
    variable emu_graph
    ## if we're doing this then the image dictates the axis ranges
    set emu_graph($graph,autorange) 0
    set emu_graph($graph,xmin) $xmin
    set emu_graph($graph,xmax) $xmax
    set emu_graph($graph,ymin) $ymin
    set emu_graph($graph,ymax) $ymax

    set emu_graph($graph,width) [image width $image]
    set emu_graph($graph,height) [image height $image]

    set emu_graph($graph,image) $image
    
    redraw $graph
}



proc configure args {
    set newargs [concat [lindex $args 0] 0 [lrange $args 1 end]]
    eval "internal_configure $newargs"
}

proc internal_configure args {
    ## rest of args is a list of option value pairs, 
    ## set emu_graph($canvas,option) to value for each option, 
    ## if args remain after last option (ie
    ## something not beginning with a - or after a --, they are returned

    variable emu_graph
    
    set graph [lindex $args 0]
    set datatag [lindex $args 1]
    set args [lrange $args 2 end]
    
    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    # if we're setting options for a data set we modify $graph
    # to include the tag to make the array entry 
    # emu_graph($graph,$tag,$option)
    if { $datatag != 0 } {
	set graph "$graph,$datatag"    
	set validoptions $emu_graph(dataoptions)
    } else {
	set validoptions $emu_graph(options)
    }	

    
    set curropt ""
    for {set i 0} { $i<[llength $args] } { incr i 2 } {
        if { [lindex $args $i] == "--" } {
            # terminating switch, return rest of args
            return [lrange $args [expr $i+1] end]
        } elseif { [regexp -- "-(.*)" [lindex $args $i] ignore curropt] } {
            # have a switch, get value and set option
            # but check first that it's kosher
            if { [lsearch $validoptions $curropt] >= 0 } {
                if { $i+1<[llength $args] } {
                    set emu_graph($graph,$curropt) [lindex $args [expr $i+1]]
 
                }
            } else {
                error "Bad option -$curropt to emu_graph\n\tuse one of $validoptions"
            }
        } else {
            ## options have run out, return rest of args
            return [lrange $args $i end]
        }
    }
}

proc destroy {graph} {
    variable emu_graph
    $emu_graph($graph,canvas) delete withtag graph$graph
    set where [lsearch $emu_graph(graphs) $graph]
    lreplace $emu_graph(graphs) $where $where
}

proc redraw {graph} {
    variable emu_graph
    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }
    # draw it if we have a canvas
    if {[info exists emu_graph($graph,canvas)]} {
	$emu_graph($graph,canvas) delete withtag graph$graph
        axes $graph
        plot_data $graph
    } else {
        error "$graph isn't associated with a canvas, use the -canvas option"
    }
}


proc is_graph {graph} {
    variable emu_graph
    return [expr [lsearch $emu_graph(graphs) $graph] >= 0]
}

proc auto_range {graph} {

    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    ## we only autorange if the option is on or if there is no range set
    if { $emu_graph($graph,autorange) ||
         !([info exists emu_graph($graph,xmin)] &&
           [info exists emu_graph($graph,xmax)] &&
           [info exists emu_graph($graph,ymin)] &&
           [info exists emu_graph($graph,ymax)]) } {


        set xyrange {{1e19 -1e19} {1e19 -1e19}}
        ## look at each dataset, find max/min for all
        foreach tag $emu_graph($graph,datasets) {

	    if { [info exists emu_graph($graph,$tag,mask)] } {
		set mask $emu_graph($graph,$tag,mask)
		set maskthresh $emu_graph($graph,$tag,maskthresh)
	    } else {
		set mask 0
		set maskthresh 0
	    }

	    if { [info exists emu_graph($graph,$tag,trackdata)] } {
		## get ranges from the data
		set yrange [$emu_graph($graph,$tag,trackdata)  				range $emu_graph($graph,$tag,trackcolumn)]
		## xrange is start and end times
		set xrange [list [$emu_graph($graph,$tag,trackdata) start] 				[$emu_graph($graph,$tag,trackdata) end]]
		set xyrange [list $xrange $yrange]
	    } elseif { [info exists emu_graph($graph,$tag,coords)] } {
		set xyrange [maxrange $xyrange  				 [range 				      $emu_graph($graph,$tag,coords) 				      $mask $maskthresh]]
	    }
        }


	set xrange [lindex $xyrange 0]
	set yrange [lindex $xyrange 1]
	
        set xextra 0
        set yextra 0
	
        set emu_graph($graph,xmin) [expr [lindex $xrange 0]-$xextra]
        set emu_graph($graph,xmax) [expr [lindex $xrange 1]+$xextra]
        set emu_graph($graph,ymin) [expr [lindex $yrange 0]-$yextra]
        set emu_graph($graph,ymax) [expr [lindex $yrange 1]+$yextra]
    }
}

proc generate_colors {} {

    set values {"EE" "88" "11"}
    foreach r $values {
	foreach g $values {
	    foreach b $values {
		lappend colors "\#$r$g$b"
	    }
	}
    }
    return $colors    
}
 
## set up emu_graph($graph,$dataset,color,$label) array
proc assign_colors {graph dataset} {
    variable emu_graph

    set colors [generate_colors]

    if {[llength $emu_graph($graph,$dataset,labels)] > 0} {
	set labels [lsort $emu_graph($graph,$dataset,labels)]
	set emu_graph($graph,$dataset,uniqlabs) {}
	set i 0
	foreach f $labels {
	    if {[lsearch -exact $f $emu_graph($graph,$dataset,uniqlabs)] == -1} {
		lappend emu_graph($graph,$dataset,uniqlabs) $f
		set emu_graph($graph,$dataset,colour,$f) [lindex $colors $i]
		incr i
		if {$i>[llength $colors]} {
		    set i 0
		}
	    }
	}
    }
}

proc plot_data {graph} {

    variable emu_graph

    if {![is_graph $graph]} { 
        error "$graph is not an emu_graph"
    }

    set canvas $emu_graph($graph,canvas)

    if {[info exists emu_graph($graph,image)]} {
	$canvas create image  	    [x2canvas $graph $emu_graph($graph,xmin)]  	    [y2canvas $graph $emu_graph($graph,ymax)]  	    -anchor nw -image $emu_graph($graph,image)  	    -tags [list graph$graph image$graph]
    }

    foreach tag $emu_graph($graph,datasets) {
        # plot the points, first delete any old ones
        $canvas delete -withtag $tag 

        set tags [list graph$graph data$graph $tag]

	if { [info exists emu_graph($graph,$tag,trackdata)] } {
	    ## get coords data from an emu trackdata object
	    set coords  		[$emu_graph($graph,$tag,trackdata) coords 		     $emu_graph($graph,$tag,trackcolumn) 		     $emu_graph($graph,xmin) $emu_graph($graph,xmax) 		     $emu_graph($graph,xfactor) $emu_graph($graph,xref) 		     $emu_graph($graph,ymin) $emu_graph($graph,ymax) 		     $emu_graph($graph,yfactor) $emu_graph($graph,yref)]
	} elseif { [info exists emu_graph($graph,$tag,coords)] } {
	    ## coords have been supplied
	    set coords  		[scale_points $graph $emu_graph($graph,$tag,coords)]
	} else {
	    set coords {}
	}
	
	# we may have a masking vector
	if { [info exists emu_graph($graph,$tag,mask)] } {
	    set mask $emu_graph($graph,$tag,mask)
	    set maskthresh $emu_graph($graph,$tag,maskthresh)
	} else {
	    set mask 0
	}
	
	## we may have labels, if so set colours but only when 
	## plotting only points
	if { [llength $emu_graph($graph,$tag,labels)] > 0 } {
	    assign_colors $graph $tag
	    set labelcolors 1
	} else {
	    set labelcolors 0
	}


	if { $emu_graph($graph,$tag,lines) } {
	    ## create the line as a single line item
	    eval "$canvas create line $coords -fill $emu_graph($graph,$tag,colour) -tag {$tags}"
	}

        for {set i 0} {$i < [llength $coords]-1} {incr i 2} {
	    ## we'll draw the point if were either not masking or if
	    ## the mask value is over the threshold
	    if { $mask == 0 ||  		     [lindex $mask [expr $i/2]] >= $maskthresh } {
		set point [lrange $coords $i [expr $i+1]]
		if { [point_in_bounds $graph $point] } {
		    
		    if { $labelcolors } {
			## find the colour for this point via its label
			set ll [lindex $emu_graph($graph,$tag,labels)  				    [expr $i/2]]
			set color $emu_graph($graph,$tag,colour,$ll)
		    } else {
			set ll {}
			set color $emu_graph($graph,$tag,colour)
		    }

		    if { $emu_graph($graph,$tag,points) } {

			set thesetags [linsert $tags end point  					   "index[expr $i/2]" "label$ll"]

			set ox [lindex $point 0]
			set oy [lindex $point 1]
			$canvas create oval  			    [expr $ox-2] [expr $oy-2] 			    [expr $ox+2] [expr $oy+2] 			    -fill $color  			    -outline black 			    -width 0  			    -tag $thesetags
		    }
		}
	    }
	}
    }
}
                   
#
# check whether point is in bounds, where point is already scaled to canvas coords
#
proc point_in_bounds {graph point} {
    variable emu_graph
    set x [lindex $point 0]
    set y [lindex $point 1]
 
    if { $x >= $emu_graph($graph,xref) && 
	 $x <= $emu_graph($graph,xref)+$emu_graph($graph,width)  &&
	 $y <= $emu_graph($graph,yref)+$emu_graph($graph,height) && 
	 $y >= $emu_graph($graph,yref) } {
	return 1 
    } else {
	return 0
    }
}


proc scale_factor {graph} {

    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    ## calculate scale factors for x and y
    set width  $emu_graph($graph,width)
    set height $emu_graph($graph,height)
    set xdelta [expr double($emu_graph($graph,xmax) - $emu_graph($graph,xmin))]
    set ydelta [expr double($emu_graph($graph,ymax) - $emu_graph($graph,ymin))]
    if {$xdelta == 0} { set xdelta 0.001 }
    if {$ydelta == 0} { set ydelta 0.001 }

    set xfactor [expr double($width)/$xdelta]
    set yfactor [expr double($height)/$ydelta]

    set emu_graph($graph,xfactor) $xfactor
    set emu_graph($graph,yfactor) $yfactor

}

proc axes {graph} {
    # generate axes for a plot
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    ## make sure we have the current scale factors etc
    auto_range $graph
    scale_factor $graph

    set x_min $emu_graph($graph,xmin)
    set x_max $emu_graph($graph,xmax)
    set y_min $emu_graph($graph,ymin)  
    set y_max $emu_graph($graph,ymax)

    set y_min_c [y2canvas $graph $y_min]
    set y_max_c [y2canvas $graph $y_max]
    set x_min_c [x2canvas $graph $x_min]
    set x_max_c [x2canvas $graph $x_max]

    # parameters affecting axis drawing
    set ticklen        $emu_graph($graph,ticklen)
    set axistextoffset $emu_graph($graph,axistextoffset)
    set nticks_x       $emu_graph($graph,nticks_x)
    set nticks_y       $emu_graph($graph,nticks_y)
    set graphfont      $emu_graph($graph,font)

    set canvas         $emu_graph($graph,canvas)

    # clear up any existing axes
    $canvas delete -withtag axis

    $canvas create rect $x_min_c $y_min_c $x_max_c $y_max_c         -outline black -tag [list graph$graph axis]
                                          
    # y-pos of tick end points and of axis tick labels
    set ticky [expr $y_min_c-$ticklen]
    set texty [expr $y_min_c+$axistextoffset]
    # put ticks and numbers on the axis 
    # starting at next nice number above x_min
    set delta_x [nicenum [expr double($x_max-$x_min)/$nticks_x] 1]
    set nicex_min [nicenum $x_min 1]

#    puts "nicex_min=$nicex_min, delta_x $delta_x, x_min $x_min,x_max $x_max"

    for {set t $nicex_min} {$t <= $x_max} {set t [expr $t+$delta_x]} {
	## it may be that $f is one tick below y_min, don't draw it if it is
	## this is because of a problem with rounding down in nicenum
	if {$t >= $x_min} {
#	    puts "t=$t, next t [expr $t+$delta_x]"
	    set x [x2canvas $graph $t]
	    $canvas create line $x $y_min_c $x $ticky  		-tag [list graph$graph axis]
	    $canvas create line $x $y_max_c $x [expr $y_max_c+$ticklen] 		-tag [list graph$graph axis]
	    # and add the label
	    $canvas create text [x2canvas $graph $t] $texty  		-text $t -font $graphfont -tag [list graph$graph axis] 		-anchor n
	}
    }

    # now the y axis
    set tickx1   [expr [x2canvas $graph $x_min]+$ticklen]
    set tickx2   [expr [x2canvas $graph $x_max]-$ticklen]
    set textx    [expr [x2canvas $graph $x_min]-$axistextoffset]

    set nicey_min [nicenum $y_min 1]   
    set delta_y [nicenum [expr double($y_max-$y_min)/$nticks_y] 1]

    for { set f $nicey_min } { $f <= $y_max } { set f [expr $f + $delta_y] } {
	## it may be that $f is one tick below y_min, don't draw it if it is
	## this is because of a problem with rounding down in nicenum
	if {$f >= $y_min} {
	    set y [y2canvas $graph $f]
	    $canvas create line [x2canvas $graph $x_min] 		$y $tickx1 $y -tag [list graph$graph axis]
	    $canvas create line [x2canvas $graph $x_max] 		$y $tickx2 $y -tag [list graph$graph axis]
	    # and add the label
	    $canvas create text $textx $y -text $f -anchor e  		-tag [list graph$graph axis] -font $graphfont
        }
    }
}

# scale_points with inlined scaling, Mark Koennecke
proc scale_points {graph coords} {
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    set result {}
    foreach {x y} $coords {
#	puts "x: $x, y: $y"
	lappend result [expr int(($x - $emu_graph($graph,xmin))                  * $emu_graph($graph,xfactor) + $emu_graph($graph,xref))]

	lappend result [expr int(($emu_graph($graph,ymax) - $y)                  * $emu_graph($graph,yfactor) + $emu_graph($graph,yref))]
    }
    return $result
}

proc bbox {graph} {
    variable emu_graph
    return [$emu_graph($graph,canvas) bbox graph$graph]
}

proc cget {graph option} { 
    variable emu_graph
    # remove leading - if present
    if { [regexp -- "-(.*)" $option ignore optname] } {
	set option $optname
    }
    # now look for it in the options store
    if {[info exists emu_graph($graph,$option)] } {
	return $emu_graph($graph,$option)
    } else {
	error "emu_graph has no configuration option $option"
    }
}


proc y2canvas {graph y} {
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    return [expr int(($emu_graph($graph,ymax) - $y)                  * $emu_graph($graph,yfactor) + $emu_graph($graph,yref))]
}

proc canvas2y {graph cy} {
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    return [expr $emu_graph($graph,ymax) -  		      ($cy - $emu_graph($graph,yref))/$emu_graph($graph,yfactor)]
}

proc canvas2x {graph cx} {
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }

    return [expr $emu_graph($graph,xmin) +  		      double($cx - $emu_graph($graph,xref))/double($emu_graph($graph,xfactor))]
}

proc x2canvas {graph x} {
    variable emu_graph

    if {![is_graph $graph]} {
        error "$graph is not an emu_graph"
    }
    return  [expr int(($x - $emu_graph($graph,xmin))                  * $emu_graph($graph,xfactor) + $emu_graph($graph,xref))]
}


## find the ranges of a list of coordinates {{x y} {x' y'} {x'' y''}...}
## returns two ranges {{xmin xmax} {ymin ymax}}
proc range {list {mask 0} {maskthresh 0}} {
    set xmax -10e64
    set xmin 10e64
    set ymax -10e64
    set ymin 10e64
    for {set i 0} {$i < [llength $list]-1} {incr i 2} {
	set x [lindex $list $i]
	set y [lindex $list [expr $i+1]]

	if { $mask == 0 ||  		 [lindex $mask [expr $i/2]] >= $maskthresh } {
	
	    if {$y > $ymax} {
		set ymax $y
	    }

	    if {$y < $ymin} {
		set ymin $y
	    }
	}
	# don't worry about the mask for x -- we still want to line up with
	# other plots 
	if {$x>$xmax} {
	    set xmax $x
	}
	
	if {$x < $xmin} {
	    set xmin $x
	}
    }
    return [list [list $xmin $xmax] [list $ymin $ymax]] 
}


## find the maxima of the sets of ranges a and b which are both {{xmin xmax} {ymin ymax}}
proc maxrange {a b} {
    return [list [maxrange-aux [lindex $a 0] [lindex $b 0]] 		[maxrange-aux [lindex $a 1] [lindex $b 1]]]
}


## find the maxima of the ranges a and b which are both {min max} pairs
proc maxrange-aux {a b} {
    # get the smallest minimum
    if {[lindex $a 0] < [lindex $b 0]} {
        set first [lindex $a 0]
    } else {
        set first [lindex $b 0]
    }
    # and the largest maximum
    if {[lindex $a 1] > [lindex $b 1]} {
        set second [lindex $a 1]
    } else {
        set second [lindex $b 1]
    }
    return [list $first $second]
}

             
## translated from C-code in Blt, who got it from:
##      Taken from Paul Heckbert's "Nice Numbers for Graph Labels" in
##      Graphics Gems (pp 61-63).  Finds a "nice" number approximately
##      equal to x.

proc nicenum {x floor} {

    if {$x == 0} { return 0 }
    
    set negative 0
    if {$x < 0} {
        set x [expr -$x]
        set negative 1
    }

    set exponX [expr floor(log10($x))]
    set fractX [expr $x/pow(10,$exponX)]; # between 1 and 10
    if {$floor} {
        if {$fractX < 1.5} {
            set nf 1.0
        } elseif {$fractX < 3.0} {
            set nf 2.0
        } elseif {$fractX < 7.0} {
            set nf 5.0
        } else {                
         set nf 10.0
        }
    } elseif {$fractX <= 1.0} {
        set nf 1.0
    } elseif {$fractX <= 2.0} {
        set nf 2.0
    } elseif {$fractX <= 5.0} {
        set nf 5.0
    } else {
        set nf 10.0
    }
    if { $negative } {
#	puts "nicenum $x -> [expr -$nf * pow(10,$exponX)]"
        return [expr -$nf * pow(10,$exponX)]
    } else {
#	puts "fractX: $fractX\nexponX: $exponX\nnf: $nf"
#	puts "nicenum $x -> [expr $nf * pow(10,$exponX)]"
	set value [expr $nf * pow(10,$exponX)]
	if {abs($value-$x) > 100} {
	    return $x
	} else {
	    return $value
	}
    }
}                

#
# put a vertical or horizontal mark on the graph 
#
proc vmark {graph x tag {color red}} {
    variable emu_graph
    if { $x >= $emu_graph($graph,xmin) && $x <= $emu_graph($graph,xmax) } {
	set tags [list graph$graph vmark $tag]
	# if there's already an item with this tag then delete it
	$emu_graph($graph,canvas) delete $tag
	set cx [x2canvas $graph $x]
	$emu_graph($graph,canvas) create line  	    $cx [y2canvas $graph $emu_graph($graph,ymin)] 	    $cx [y2canvas $graph $emu_graph($graph,ymax)] 	    -fill $color -tags $tags
    }
}

proc hmark {graph y tag {color red}} {
    variable emu_graph
    if { $y >= $emu_graph($graph,ymin) && $y <= $emu_graph($graph,ymax) } {
	# if there's already an item with this tag then delete it
	$emu_graph($graph,canvas) delete $tag
	set tags [list graph$graph vmark $tag]
	set cy [y2canvas $graph $y]
	$emu_graph($graph,canvas) create line  	    [x2canvas $graph $emu_graph($graph,xmin)] $cy 	    [x2canvas $graph $emu_graph($graph,xmax)] $cy 	    -fill $color -tags $tags
    }
}

proc clearmark {graph tag} {
    variable emu_graph
    $emu_graph($graph,canvas) delete $tag
}


proc movevmark {graph tag newx} {
    variable emu_graph
    set cx [x2canvas $graph $newx]
    $emu_graph($graph,canvas) coords $tag  	    $cx [y2canvas $graph $emu_graph($graph,ymin)] 	    $cx [y2canvas $graph $emu_graph($graph,ymax)]
}

proc movehmark {graph tag newy} {
    variable emu_graph
    set cy [y2canvas $graph $newy]
    $emu_graph($graph,canvas) coords $tag  	    [x2canvas $graph $emu_graph($graph,xmin)] $cy 	    [x2canvas $graph $emu_graph($graph,xmax)] $cy }


}

    #source ./emu-graph1.1.2/tcl/graph.tcl
} else {
    #source m:\\snit1.0\\snit.tcl
    #source m:\\snit1.0\\pkgIndex.tcl
    
    #source m:\\pdf4tcl05\\glyphnames.tcl
    #source m:\\pdf4tcl05\\metrics.tcl
    #source m:\\pdf4tcl05\\pdf4tcl.tcl
    #source m:\\pdf4tcl05\\pkgIndex.tcl

    #source m:\\emu-graph1.1.2\\tcl\\graph.tcl
}

if {$tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
    #source /nfs/depot/cce_u1/scott/mhscott/emu-graph1.1.2/tcl/graph.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/rgb.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/comb.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/shellTri.tcl
} else  {
    #source m:\\emu-graph1.1.2\\tcl\\graph.tcl
    #source m:\\tclUtility\\rgb.tcl
    #source m:\\tclUtility\\comb.tcl
    #source m:\\tclUtility\\shellTri.tcl
}

package require pdf4tcl 0.5


# Start MHS Code

set toggleFrame .fBent

proc SoftwareTitle {} {
   return "Alaska DOT&PF Pile Extension Pier Pushover Program"
}

proc VersionNumber {} {
   return "2.0"
}

#####

proc ModelIsDirty {} {
    global modelChangedSinceSave
    set modelChangedSinceSave 1
}

proc IsModelDirty {} {
    global modelChangedSinceSave
    return $modelChangedSinceSave
}

proc ModelIsClean {} {
    global modelChangedSinceSave
    set modelChangedSinceSave 0
}

ModelIsClean

#####

proc SectionIsDirty {} {
    global sectionChangedSinceSave
    set sectionChangedSinceSave 1
}

proc IsSectionDirty {} {
    global sectionChangedSinceSave
    return $sectionChangedSinceSave
}

proc SectionIsClean {} {
    global sectionChangedSinceSave
    set sectionChangedSinceSave 0
}

SectionIsClean

#####

# Window title
wm title . "[SoftwareTitle], v[VersionNumber]"

# Set to a random name that can be overwritten
set bridgeName "Bridge[expr abs([clock clicks])]"
set engineerName "John Doe, P.E."

proc Assign_colors {} {
    global colors
    global colorsTMP

    foreach var [array names colorsTMP] {
	set colors($var) $colorsTMP($var)
    }
}

proc AssignTMP_colors {} {
    global colors
    global colorsTMP

    foreach var [array names colors] {
	set colorsTMP($var) $colors($var)
    }
}

set colors(steel,default) #834b43
set colors(concrete,default) grey
set colors(ground,default) brown
set colors(pile,default) #aa4b43

set colors(steel) $colors(steel,default)
set colors(concrete) $colors(concrete,default)
set colors(ground) $colors(ground,default)
set colors(pile) $colors(pile,default)
AssignTMP_colors

proc ChooseColor {whichColor} {

    upvar $whichColor color

    set tmpColor [tk_chooseColor -initialcolor $color]

    if {[string length $tmpColor] > 0} {
       set color $tmpColor
    }
}

proc RestoreDefaultColors {} {
    global colors
    global colorsTMP    

    set colorsTMP(steel) $colors(steel,default)
    set colorsTMP(concrete) $colors(concrete,default)
    set colorsTMP(ground) $colors(ground,default)
    set colorsTMP(pile) $colors(pile,default)

    Assign_colors
}

set units(ft) 1.0
set units(kip) 1.0
set units(sec) 1.0
set units(lb) [expr $units(kip)/1000.0]
set units(in) [expr $units(ft)/12.0]
set units(ksi) [expr $units(kip)/pow($units(in),2)]
set units(psi) [expr $units(lb)/pow($units(in),2)]
set units(pci) [expr $units(lb)/pow($units(in),3)]
set units(pcf) [expr $units(lb)/pow($units(ft),3)]
set units(rad) 1.0
set units(deg) [expr 2*asin(1.0)/180.0]

proc isValidDouble {value} {
   return [expr [string is double $value] && [string length $value] > 0]
}

proc isValidInt {value} {
   return [expr [string is integer $value] && [string length $value] > 0]
}

set whichSection 1

proc Assign_analysisProps {} {
   global analysisProps
   global analysisPropsTMP

   foreach var [array names analysisPropsTMP] {
      if {[info exists analysisProps($var)] && $analysisProps($var) != $analysisPropsTMP($var)} {
         ModelIsDirty
      }
      set analysisProps($var) $analysisPropsTMP($var)
   }

   Assign_viewProps
}

proc Assign_viewProps {} {
   global viewProps
   global viewPropsTMP

   # Don't want these to affect "ModelIsDirty" above
   foreach var [array names viewPropsTMP] {
      set viewProps($var) $viewPropsTMP($var)
   }
}

proc AssignTMP_analysisProps {} {
    global analysisProps
    global analysisPropsTMP

    foreach var [array names analysisProps] {
	set analysisPropsTMP($var) $analysisProps($var)
    }

    AssignTMP_viewProps
}

proc AssignTMP_viewProps {} {
   global viewProps
   global viewPropsTMP

   foreach var [array names viewProps] {
      set viewPropsTMP($var) $viewProps($var)
   }
}

set analysisProps(lateralDisp,T) 2.0
set analysisProps(lateralDisp,L) 3.0
set analysisProps(soilStructure) 1
set analysisProps(pDelta)        1
set analysisProps(postPeak)      80
set analysisProps(lateralLoad) 1 ;# 1 = distributed, 0 = concentrated
set analysisProps(overstrength)      1.0
set analysisProps(subtractWater) 1
set analysisProps(allElastic) 0
set analysisProps(massDL) 1.0
set analysisProps(damping) 5
set analysisProps(stiffnessProp) 1 ;# 1 = last committed, 0 = initial
set analysisProps(timestep) 0.02
set analysisProps(tFinal) 30.0

set viewProps(drawD)         0
set viewProps(drawM)         1
set viewProps(drawV)         0

AssignTMP_analysisProps

set staticAnalysisWindowOpen 0
proc DefineStaticAnalysis {w} {
    global staticAnalysisWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$staticAnalysisWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Pushover Analysis Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set staticAnalysisWindowOpen 1
    bind $w <Destroy> {set staticAnalysisWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.radioLoad -text "Lateral Load"
    grid $w.input.radioLoad -row $row -column 0 -sticky e
    radiobutton $w.input.loadDist -variable analysisPropsTMP(lateralLoad) -text Distributed -value 1
    grid $w.input.loadDist -row $row -column 1
    radiobutton $w.input.loadConc -variable analysisPropsTMP(lateralLoad) -text Concentrated -value 0
    grid $w.input.loadConc -row $row -column 2

    incr row

    label $w.input.labelDrift -text "Max Displacement Transverse"
    grid $w.input.labelDrift -row $row -column 0 -sticky e
    entry $w.input.entryDrift -width 5 -textvariable analysisPropsTMP(lateralDisp,T)
    bind $w.input.entryDrift <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryDrift -row $row -column 1
    label $w.input.unitDrift -text "ft"
    grid $w.input.unitDrift -row $row -column 2 -sticky w

    incr row

    label $w.input.labelDriftL -text "Max Displacement Longitudinal"
    grid $w.input.labelDriftL -row $row -column 0 -sticky e
    entry $w.input.entryDriftL -width 5 -textvariable analysisPropsTMP(lateralDisp,L)
    bind $w.input.entryDriftL <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryDriftL -row $row -column 1
    label $w.input.unitDriftL -text "ft"
    grid $w.input.unitDriftL -row $row -column 2 -sticky w

    incr row

    label $w.input.labelPeak -text "Post Peak Capacity"
    grid $w.input.labelPeak -row $row -column 0 -sticky e
    entry $w.input.entryPeak -width 5 -textvariable analysisPropsTMP(postPeak)
    bind $w.input.entryPeak <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryPeak -row $row -column 1
    label $w.input.unitPeak -text "\%"
    grid $w.input.unitPeak -row $row -column 2 -sticky w

    incr row





    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

set dynamicAnalysisWindowOpen 0
proc DefineDynamicAnalysis {w} {
    global dynamicAnalysisWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$dynamicAnalysisWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Time History Analysis Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set dynamicAnalysisWindowOpen 1
    bind $w <Destroy> {set dynamicAnalysisWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.labeldt -text "Time Step"
    grid $w.input.labeldt -row $row -column 0 -sticky e
    entry $w.input.entrydt -width 5 -textvariable analysisPropsTMP(timestep)
    bind $w.input.entrydt <Return> "DefineAnalysis_OK $w"
    grid $w.input.entrydt -row $row -column 1
    label $w.input.unitdt -text "sec"
    grid $w.input.unitdt -row $row -column 2 -sticky w

    incr row

    label $w.input.labeltf -text "Time Final"
    grid $w.input.labeltf -row $row -column 0 -sticky e
    entry $w.input.entrytf -width 5 -textvariable analysisPropsTMP(tFinal)
    bind $w.input.entrytf <Return> "DefineAnalysis_OK $w"
    grid $w.input.entrytf -row $row -column 1
    label $w.input.unittf -text "sec"
    grid $w.input.unittf -row $row -column 2 -sticky w

    incr row

    label $w.input.labelMDL -text "Mass-Dead Load Multiplier"
    grid $w.input.labelMDL -row $row -column 0 -sticky e
    entry $w.input.entryMDL -width 5 -textvariable analysisPropsTMP(massDL)
    bind $w.input.entryMDL <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryMDL -row $row -column 1
    label $w.input.unitMDL -text ""
    grid $w.input.unitMDL -row $row -column 2 -sticky w

    incr row

    label $w.input.labeld -text "Damping Ratio"
    grid $w.input.labeld -row $row -column 0 -sticky e
    entry $w.input.entryd -width 5 -textvariable analysisPropsTMP(damping)
    bind $w.input.entryd <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryd -row $row -column 1
    label $w.input.unitd -text "\%"
    grid $w.input.unitd -row $row -column 2 -sticky w
    label $w.input.labeldamp -text "Stiffness Proportional"
    grid $w.input.labeldamp -row $row -column 3 -columnspan 2


    incr row

    #label $w.input.labeldamp -text "Stiffness Proportional"
    #grid $w.input.labeldamp -row $row -column 0 -sticky e
    radiobutton $w.input.tangent -variable analysisPropsTMP(stiffnessProp) -text Current -value 1
    grid $w.input.tangent -row $row -column 3
    radiobutton $w.input.initial -variable analysisPropsTMP(stiffnessProp) -text Initial -value 0
    grid $w.input.initial -row $row -column 4

    pack $w.input -side top



    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

set modelingWindowOpen 0
proc DefineModelingOptions {w} {
    global modelingWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$modelingWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Modeling Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set modelingWindowOpen 1
    bind $w <Destroy> {set modelingWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.labelOver -text "Overstrength Factor"
    grid $w.input.labelOver -row $row -column 0 -sticky e
    entry $w.input.entryOver -width 5 -textvariable analysisPropsTMP(overstrength)
    bind $w.input.entryOver <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryOver -row $row -column 1
    label $w.input.unitOver -text ""
    grid $w.input.unitOver -row $row -column 2 -sticky w

    incr row

    label $w.input.radioSSI -text "Soil-Structure Interaction"
    grid $w.input.radioSSI -row $row -column 0 -sticky e
    radiobutton $w.input.ssiOn -variable analysisPropsTMP(soilStructure) -text On -value 1
    grid $w.input.ssiOn -row $row -column 1
    radiobutton $w.input.ssiOff -variable analysisPropsTMP(soilStructure) -text Off -value 0
    grid $w.input.ssiOff -row $row -column 2

    incr row

    label $w.input.radioPD -text "P-Delta Analysis"
    grid $w.input.radioPD -row $row -column 0 -sticky e
    radiobutton $w.input.pdOn -variable analysisPropsTMP(pDelta) -text On -value 1
    grid $w.input.pdOn -row $row -column 1
    radiobutton $w.input.pdOff -variable analysisPropsTMP(pDelta) -text Off -value 0
    grid $w.input.pdOff -row $row -column 2

    incr row

    label $w.input.radioWD -text "Soil Buoyant Weight below GWT"
    grid $w.input.radioWD -row $row -column 0 -sticky e
    radiobutton $w.input.wdOn -variable analysisPropsTMP(subtractWater) -text On -value 1
    grid $w.input.wdOn -row $row -column 1
    radiobutton $w.input.wdOff -variable analysisPropsTMP(subtractWater) -text Off -value 0
    grid $w.input.wdOff -row $row -column 2

    incr row

    label $w.input.radioAE -text "All Elastic Materials"
    grid $w.input.radioAE -row $row -column 0 -sticky e
    radiobutton $w.input.aeOn -variable analysisPropsTMP(allElastic) -text On -value 1
    grid $w.input.aeOn -row $row -column 1
    radiobutton $w.input.aeOff -variable analysisPropsTMP(allElastic) -text Off -value 0
    grid $w.input.aeOff -row $row -column 2

    incr row

    label $w.input.radioDD -text "Draw Displaced Shape"
    grid $w.input.radioDD -row $row -column 0 -sticky e
    radiobutton $w.input.ddOn -variable viewPropsTMP(drawD) -text On -value 1
    grid $w.input.ddOn -row $row -column 1
    radiobutton $w.input.ddOff -variable viewPropsTMP(drawD) -text Off -value 0
    grid $w.input.ddOff -row $row -column 2

    incr row

    label $w.input.radioMD -text "Draw Moment Diagram"
    grid $w.input.radioMD -row $row -column 0 -sticky e
    radiobutton $w.input.mdOn -variable viewPropsTMP(drawM) -text On -value 1
    grid $w.input.mdOn -row $row -column 1
    radiobutton $w.input.mdOff -variable viewPropsTMP(drawM) -text Off -value 0
    grid $w.input.mdOff -row $row -column 2

    incr row

    label $w.input.radioVD -text "Draw Shear Diagram"
    grid $w.input.radioVD -row $row -column 0 -sticky e
    radiobutton $w.input.vdOn -variable viewPropsTMP(drawV) -text On -value 1
    grid $w.input.vdOn -row $row -column 1
    radiobutton $w.input.vdOff -variable viewPropsTMP(drawV) -text Off -value 0
    grid $w.input.vdOff -row $row -column 2

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineAnalysis_Cancel {w} {
    AssignTMP_analysisProps

    destroy $w
}

proc DefineAnalysis_OK {w} {
    
    set ok [CheckAnalysis]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_analysisProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckAnalysis {} {
    global analysisPropsTMP

    set bcap $analysisPropsTMP(lateralDisp,T)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "maximum displacement transverse"
    }

    set bcap $analysisPropsTMP(lateralDisp,L)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "maximum displacement longitudinal"
    }

    set dcap $analysisPropsTMP(postPeak)
    if {![isValidDouble $dcap] || $dcap < 0.0 || $dcap > 100.0} {
	return "post peak capacity"
    }

    set dcap $analysisPropsTMP(damping)
    if {![isValidDouble $dcap] || $dcap < 0.0 || $dcap > 100.0} {
	return "damping ratio"
    }

    set dcap $analysisPropsTMP(overstrength)
    if {![isValidDouble $dcap] || $dcap <= 0.0} {
	return "overstrength factor"
    }

    set dcap $analysisPropsTMP(massDL)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "mass-dead load multiplier"
    }

    set dcap $analysisPropsTMP(timestep)
    if {![isValidDouble $dcap] || $dcap <= 0.0} {
	return "time step"
    }

    set bcap $analysisPropsTMP(tFinal)
    if {![isValidDouble $bcap] || $bcap < 0.0} {
	return "time final"
    }
}


proc Assign_sectionProps {} {
    global sectionProps
    global sectionPropsTMP

    foreach var [array names sectionPropsTMP] {
       if {[info exists sectionProps($var)] && $sectionProps($var) != $sectionPropsTMP($var)} {
          ModelIsDirty
          SectionIsDirty
       }
       set sectionProps($var) $sectionPropsTMP($var)
    }

    if {$sectionProps(pileSameAsColumn) == 1} {
	foreach var "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	    set sectionProps($var,2) $sectionProps($var,1)
	}
    }
}

proc AssignTMP_sectionProps {} {
    global sectionProps
    global sectionPropsTMP

    foreach var [array names sectionProps] {
	set sectionPropsTMP($var) $sectionProps($var)
    }
}

proc BarArea {barNum} {

   global units
   set in $units(in)

   set pi [expr acos(-1.0)]
   if {$barNum <= 8} {
      set diam [expr {0.125*$barNum*$in}]
      return [expr {0.25*$pi*$diam*$diam}]
   }
   if {$barNum == 9}  {return [expr {1.00*$in*$in}]}
   if {$barNum == 10} {return [expr {1.27*$in*$in}]}
   if {$barNum == 11} {return [expr {1.56*$in*$in}]}
   if {$barNum == 14} {return [expr {2.25*$in*$in}]}
   if {$barNum == 18} {return [expr {4.00*$in*$in}]}
}

proc BarDiam {barNum} {

   global units
   set in $units(in)

   if {$barNum <= 8} {
       return [expr {0.125*$barNum*$in}]
   }
   if {$barNum == 9}  {return [expr {1.128*$in}]}
   if {$barNum == 10} {return [expr {1.270*$in}]}
   if {$barNum == 11} {return [expr {1.410*$in}]}
   if {$barNum == 14} {return [expr {1.693*$in}]}
   if {$barNum == 18} {return [expr {2.257*$in}]}
}

# Column
set sectionProps(Dj,1) 36.0
set sectionProps(tj,1) 0.75
set sectionProps(ds,1) 11
set sectionProps(cover,1) 2.0
set sectionProps(Nbars,1) 16
set sectionProps(dshoop,1) 5
set sectionProps(fyhoop,1) 60.0
set sectionProps(sphoop,1) 3.0
set sectionProps(noConcrete,1) 0

set sectionProps(pileSameAsColumn) 1
if {$sectionProps(pileSameAsColumn) == 1} {
    foreach prop "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	set sectionProps($prop,2) $sectionProps($prop,1)
    }
}
set sectionProps(changePile) 5.0
set sectionProps(MKductility) 25.0
set sectionProps(NMductility) 1.0
AssignTMP_sectionProps

proc CISSSection {} {
   return 1
}
proc GapSection {} {
   return 2
}
proc PileSection {} {
   return 3
}

set sectionWindowOpen 0
proc DefineSection {w} {
    global sectionWindowOpen
    global whichSection

    # If already open, do not attempt to open a duplicate
    if {$sectionWindowOpen == 1} {raise $w; return}

    # Create new window
    if {$whichSection == 2} {
	createTopLevel $w "Pile Section"
    } else {
	createTopLevel $w "Column Section"
    }

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    # Create "Materials" pull down menu
    menubutton $w.mbar.materials -text Materials -menu $w.mbar.materials.menu
    pack $w.mbar.materials -side left
    set m [menu $w.mbar.materials.menu]

    $m add command -label "Concrete" -command "DefineConcrete .concrete"
    $m add command -label "Reinforcing Steel" -command "DefineRebarSteel .rebarSteel"
    $m add command -label "Steel Shell" -command "DefineJacketSteel .jacketSteel"

    # Create "Analysis" pull down menu
    menubutton $w.mbar.analysis -text Analysis -menu $w.mbar.analysis.menu
    pack $w.mbar.analysis -side left
    set m [menu $w.mbar.analysis.menu]

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    if {$whichSection == 1} {
	$m add cascade -label "Moment-Curvature" -menu $m.mkmenu
	set m2 [menu $m.mkmenu]
	$m2 add command -label "Gap Section" -command "DefineMomentCurvature .moment-curvature $withoutShell"
	$m2 add command -label "CISS Section" -command "DefineMomentCurvature .moment-curvature $withShell"
    } else {
	$m add command -label "Moment-Curvature" -command "DefineMomentCurvature .moment-curvature $pile"
    }
    if {$whichSection == 1} {
	$m add cascade -label "N-M Interaction" -menu $m.nmmenu
	set m2 [menu $m.nmmenu]
	$m2 add command -label "Gap Section" -command "DefineNMAnalysis .nm $withoutShell"
	$m2 add command -label "CISS Section" -command "DefineNMAnalysis .nm $withShell"
    } else {
	$m add command -label "N-M Interaction" -command "DefineNMAnalysis .nm $pile"
    }

    set text(0)  "The input values are the parameters required to simulate the respone of a CISS section by fiber discretization.  Updates to these parameters will cause the section to be re-drawn automatically with the current input values."
    set text(1) "Double-clicking section items, or selecting from the Materials menu, will bring up an edit menu for the associated material properties."
    set text(2) "Two section analysis options (moment-curvature and axial-moment interaction) are available from the Analysis menu."

    createHelp $w $w.mbar section text

    set sectionWindowOpen 1
    bind $w <Destroy> {set sectionWindowOpen 0}

    AssignTMP_sectionProps

    frame $w.input

    set myFrame [frame $w.input.shell]
    label $myFrame.title -text "Steel Shell" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Diameter" Dj in "Thickness" tj in} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }

    set myFrame [frame $w.input.longrf]
    label $myFrame.title -text "Longitudinal R/F" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Clear Cover" cover in "Number of Bars" Nbars ""} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }
    label $myFrame.labelbsm -text "Bar Size"
    grid $myFrame.labelbsm -row $row -column 0 -sticky e
    set menu [tk_optionMenu $myFrame.barsizemenua sectionPropsTMP(ds,$whichSection) junk]
    $menu delete 0
    set i 0
    foreach size {3 4 5 6 7 8 9 10 11 14 18} {
	$menu insert $i radiobutton -label $size -variable sectionPropsTMP(ds,$whichSection) -command "DrawSection $w.show.canvas"
	incr i
    }
    grid $myFrame.barsizemenua -row $row -column 1 -columnspan 2

    set myFrame [frame $w.input.hooprf]
    label $myFrame.title -text "Transverse R/F" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Spacing" sphoop in "Yield Stress" fyhoop ksi} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }
    label $myFrame.labelbsm -text "Bar Size"
    grid $myFrame.labelbsm -row $row -column 0 -sticky e
    set menu [tk_optionMenu $myFrame.barsizemenua sectionPropsTMP(dshoop,$whichSection) junk]
    $menu delete 0
    set i 0
    foreach size {3 4 5 6 7 8 9 10 11 14 18} {
	$menu insert $i radiobutton -label $size -variable sectionPropsTMP(dshoop,$whichSection) -command "DrawSection $w.show.canvas"
	incr i
    }
    grid $myFrame.barsizemenua -row $row -column 1 -columnspan 2

    pack $w.input.shell $w.input.longrf $w.input.hooprf -side left -ipadx 5 -ipady 5 -anchor n

    pack $w.input -side top

    
    set myFrame [frame $w.noconcrete]

    incr row
    checkbutton $myFrame.pileConc -text "No concrete in section" -variable sectionPropsTMP(noConcrete,$whichSection) -command "EnableRCSectionEntries $w $whichSection; DrawSection $w.show.canvas"
    grid $myFrame.pileConc -row $row -column 0 -sticky ew -columnspan 3
	
    pack $w.noconcrete -side top

    EnableRCSectionEntries $w $whichSection
    
    if {$whichSection == 2} {
	incr row
	set myFrame [frame $w.pile]

	label $myFrame.ltrans -text "Change at depth"
	grid $myFrame.ltrans -row $row -column 0 -sticky e
	entry $myFrame.etrans -width 5 -textvariable sectionPropsTMP(changePile)
	bind $myFrame.etrans <Return> "DefineSection_OK $w"
	grid $myFrame.etrans -row $row -column 1
	label $myFrame.utrans -text "ft"
	grid $myFrame.utrans -row $row -column 2 -sticky w

	incr row

	checkbutton $myFrame.pile -text "Make pile section same as column" -variable sectionPropsTMP(pileSameAsColumn) -command "EnablePileSectionEntries $w; DrawSection $w.show.canvas"
	grid $myFrame.pile -row $row -column 0 -sticky ew -columnspan 3

	pack $w.pile -side top

	EnablePileSectionEntries $w
    }

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSection_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSection_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top    

    frame $w.show
    canvas $w.show.canvas -height 400 -width 400
    grid $w.show.canvas -row 0 -column 2 -rowspan $row
    pack $w.show -side top

    DrawSection $w.show.canvas
}

proc EnableRCSectionEntries {w whichSection} {
    global sectionPropsTMP

    if {$sectionPropsTMP(noConcrete,$whichSection) == 0} {
	set state normal
    } else {
	set state disabled
    }

    $w.input.longrf.entry1 config -state $state
    $w.input.longrf.entry2 config -state $state
    $w.input.longrf.barsizemenua config -state $state

    $w.input.hooprf.entry1 config -state $state
    $w.input.hooprf.entry2 config -state $state
    $w.input.hooprf.barsizemenua config -state $state    
}

proc EnablePileSectionEntries {w} {
    global sectionPropsTMP

    if {$sectionPropsTMP(pileSameAsColumn) == 1} {
	set state disabled
    } else {
	set state normal
    }

    $w.input.shell.entry1 config -state $state
    $w.input.shell.entry2 config -state $state

    $w.input.longrf.entry1 config -state $state
    $w.input.longrf.entry2 config -state $state
    $w.input.longrf.barsizemenua config -state $state

    $w.input.hooprf.entry1 config -state $state
    $w.input.hooprf.entry2 config -state $state
    $w.input.hooprf.barsizemenua config -state $state

    $w.noconcrete.pileConc config -state $state
    $w.pile.etrans config -state $state
}

proc DefineSection_Cancel {w} {
    AssignTMP_sectionProps

    destroy $w
}

proc DefineSection_OK {w} {

    set ok [CheckSection]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    global sectionProps
    global sectionPropsTMP

    if {$sectionPropsTMP(pileSameAsColumn) == 1 && $sectionProps(pileSameAsColumn) == 0} {
	foreach tmp "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	    set sectionPropsTMP($tmp,2) $sectionPropsTMP($tmp,1)
	}
    }

    Assign_sectionProps

    global hingeProps
    global hingePropsTMP
    Computelp
    set hingeProps(lp) $hingePropsTMP(lp)

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSection {} {
    global sectionPropsTMP
    global whichSection

    set Dj $sectionPropsTMP(Dj,$whichSection)
    if {![isValidDouble $Dj] || $Dj <= 0.0} {
	return "shell diameter"
    }

    set tj $sectionPropsTMP(tj,$whichSection)
    if {![isValidDouble $tj] || $tj < 0.0} {
	return "shell thickness"
    }

    if {$tj <= 0.0 && $sectionPropsTMP(noConcrete,$whichSection) == 1} {
	return "no concrete and no steel shell"
    }

    set cover $sectionPropsTMP(cover,$whichSection)
    if {![isValidDouble $cover] || $cover < 0.0} {
	return cover
    }

    set Nbars $sectionPropsTMP(Nbars,$whichSection)
    if {![isValidInt $Nbars] || $Nbars < 0} {
	return Nbars
    }

    set ds $sectionPropsTMP(ds,$whichSection)
    # Not really needed since it's a menu choice
    if {![isValidInt $ds]} {
	return ds
    }

    set fyhoop $sectionPropsTMP(fyhoop,$whichSection)
    if {![isValidDouble $fyhoop] || $fyhoop <= 0.0} {
	return "hoop yield strength"
    }

    set tj $sectionPropsTMP(sphoop,$whichSection)
    if {![isValidDouble $tj] || $tj <= 0.0} {
	return "hoop spacing"
    }

    set dshoop $sectionPropsTMP(dshoop,$whichSection)
    # Not really needed since it's a menu choice
    if {![isValidInt $dshoop]} {
	return dshoop
    }

    global columnPropsTMP
    set L $columnPropsTMP(L)
    set changePile $sectionPropsTMP(changePile)
    if {![isValidDouble $changePile] || $changePile < -$L} {
	return "depth of column/pile section interface"
    }
}

# A proc to draw the CISS section
# input: c -- canvas to which objects will be drawn
proc DrawSection {c {color on}} {
    global sectionPropsTMP
    global whichSection
    global colors

    set sectionToDraw $whichSection
    if {$sectionPropsTMP(pileSameAsColumn) == 1} {
	set sectionToDraw 1
    }

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]
    set dim [expr {0.95*(0.5*$width)}]

    set Dj $sectionPropsTMP(Dj,$sectionToDraw)
    if {![isValidDouble $Dj] || $Dj == 0.0} {return}
    set tj $sectionPropsTMP(tj,$sectionToDraw)
    if {![isValidDouble $tj]} {return}
    set cover $sectionPropsTMP(cover,$sectionToDraw)
    if {![isValidDouble $cover]} {return}

    set Nbars $sectionPropsTMP(Nbars,$sectionToDraw)
    if {![isValidInt $Nbars] || $Nbars < 0} {return}
    set ds $sectionPropsTMP(ds,$sectionToDraw)
    if {![isValidInt $ds]} {return}
    set dshoop $sectionPropsTMP(dshoop,$sectionToDraw)
    if {![isValidInt $dshoop]} {return}

    global units
    set in $units(in)

    clearCanvas $c

    set concreteColor $colors(concrete)
    set steelColor $colors(steel)
    if {$color != "on"} {
        set concreteColor white
        set steelColor white
    }
    set drawRC 1
    #if {$whichSection != 1 && $sectionPropsTMP(pileNoConcrete) == 1 && $sectionPropsTMP(pileSameAsColumn) == 0} 
    if {$sectionPropsTMP(noConcrete,$whichSection) == 1} {
	set drawRC 0
	set concreteColor white
    }

    $c create oval -$dim -$dim $dim $dim -fill $steelColor -tag jacket 
    set dim1 [expr {-$dim+(2*$dim)*$tj/$Dj}]
    set dim2 [expr -$dim1]
    $c create oval $dim1 $dim1 $dim2 $dim2 -fill $concreteColor -tag concrete

    # Draw transverse r/f
    if {$drawRC} {
	set dim3 [expr {-$dim + (2*$dim)/$Dj*($tj+$cover)}]
	$c create oval $dim3 $dim3 [expr -$dim3] [expr -$dim3]

	set dim3 [expr {-$dim + (2*$dim)/$Dj*($tj+$cover+[BarDiam $dshoop]/$in)}]
	$c create oval $dim3 $dim3 [expr -$dim3] [expr -$dim3]
    }
    
    set pi [expr {acos(-1.0)}]
    set dtheta [expr {2*$pi/$Nbars}]
    set radius [expr {$dim-(2*$dim)*($tj+$cover+[BarDiam $dshoop]/$in+0.5*[BarDiam $ds]/$in)/$Dj}]    
    set cor [expr {0.5*[BarDiam $ds]/$in*(2*$dim)/$Dj}]

    # Draw longitudinal rebars
    for {set i 0} {$drawRC && $i < $Nbars} {incr i} {
        set theta [expr {$i*$dtheta}]
        set dx [expr {$radius*cos($theta)}]
        set dy [expr {$radius*sin($theta)}]
        $c create oval [expr {$cor+$dx}] [expr {$cor+$dy}] [expr {-$cor+$dx}] [expr {-$cor+$dy}] -fill $steelColor -tag rebar
    }

    $c bind jacket <Double-Button-1> {DefineJacketSteel .jacketSteel}
    $c bind rebar <Double-Button-1> {DefineRebarSteel .rebarSteel}
    $c bind concrete <Double-Button-1> {DefineConcrete .concrete}

    $c move all [expr {0.5*$width}] [expr {0.5*$height}]
}

proc DefineOpenSeesSection {ciss gap pile} {
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP

    set asj 1.0

    set tagUnconf 1
    set tagCISS 2
    set tagHoop 3
    set tagPile 4
    OpenSeesConcrete $tagUnconf $tagCISS $tagHoop $tagPile

    
    set tagRebar 5
    OpenSeesRebarSteel $tagRebar

    set tagJacket 6
    OpenSeesJacketSteel $tagJacket
    
    # Define cross-section for nonlinear columns
    # ------------------------------------------
    
    set Dj $sectionPropsTMP(Dj,1)
    set Dj [expr {$Dj*$in}]
    set tj $sectionPropsTMP(tj,1)
    set tj [expr {$tj*$in}]
    set cover $sectionPropsTMP(cover,1)
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(ds,1)
    set As [BarArea $ds]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    set Nbars $sectionPropsTMP(Nbars,1)
    set halfBar [expr 0.5*[BarDiam $ds]]

    set discretize2D 1
    #set discretize2D 0
    set Nl 20

	set coverTag $tagCISS
	# If there is no shell, the cover concrete is unconfined
    if {$tj <= 0.0} {
       set coverTag $tagUnconf
    }

    section Fiber $ciss {

	if {$sectionPropsTMP(noConcrete,1) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $tagCISS 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber $yi 0 $Ai $tagCISS
		    fiber [expr -$yi] 0 $Ai $tagCISS
		}
	    }
		
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0

	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	}
	
	# Steel shell
	if {$tj > 0.0} {
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  
	}
    }    
    
    global hingeProps
    set coreTag $tagCISS
    if {$hingeProps(coreConfined) == 0} {
       set coreTag $tagHoop
    }
    set coverTag $coreTag
    if {$hingeProps(coverConfined) == 0} {
       set coverTag $tagUnconf
    }

    section Fiber $gap {

	if {$sectionPropsTMP(noConcrete,1) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $coreTag 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber  $yi 0 $Ai $coreTag
		    fiber [expr -$yi] 0 $Ai $coreTag
		}
	    }
	    
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0
	    
	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	} else {
	    # Make the gap section an empty steel shell if there is no concrete;
	    # otherwise, big numerical problems will occur!
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  	    
	}
    }


    # Pile section
    set Dj $sectionPropsTMP(Dj,2)
    set Dj [expr {$Dj*$in}]
    set tj $sectionPropsTMP(tj,2)
    set tj [expr {$tj*$in}]
    set cover $sectionPropsTMP(cover,2)
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(ds,2)
    set As [BarArea $ds]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    set Nbars $sectionPropsTMP(Nbars,2)
    set halfBar [expr 0.5*[BarDiam $ds]]

	set coverTag $tagPile
	# If there is no shell, the cover concrete is unconfined
    if {$tj <= 0.0} {
       set coverTag $tagUnconf
    }
	
    section Fiber $pile {
	
	if {$sectionPropsTMP(noConcrete,2) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $tagPile 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber  $yi 0 $Ai $tagPile
		    fiber [expr -$yi] 0 $Ai $tagPile
		}
	    }
		
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0
		
	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	}

	# Steel shell
	if {$tj > 0.0} {
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  
	}
    }
}

set nmWindowOpen 0
proc DefineNMAnalysis {w shell} {
    global nmWindowOpen

    if {$nmWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Axial-Moment Interaction"

    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the curvature ductility at which the moment is reported for varying levels of axial load."

    createHelp $w $w.mbar nmInteraction text


    set nmWindowOpen 1
    bind $w <Destroy> {set nmWindowOpen 0}
    


    frame $w.input

    label $w.input.labelduct -text "Target Ductility"
    grid $w.input.labelduct -row 1 -column 0
    entry $w.input.duct -width 5 -textvariable sectionPropsTMP(NMductility)
    grid $w.input.duct -row 1 -column 1

    button $w.input.analyze -text "Analyze" -command "NMAnalysis .nmAnalysis $shell"
    grid $w.input.analyze -row 2 -column 0 -columnspan 2 -sticky ew

    button $w.input.close -text "Close" -command "destroy $w"
    grid $w.input.close -row 3 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top

}

set OpenSeesNMResults(withShell) ""
set OpenSeesNMResults(withoutShell) ""
set OpenSeesNMResults(pile) ""
proc OpenSeesNMAnalysis {shell} {
    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    global OpenSeesNMResults
    if {![IsSectionDirty] && $shell == $withShell && [llength $OpenSeesNMResults(withShell)] > 0} {
	return $OpenSeesNMResults(withShell)
    }
    if {![IsSectionDirty] && $shell == $withoutShell && [llength $OpenSeesNMResults(withoutShell)] > 0} {
	return $OpenSeesNMResults(withoutShell)
    }
    if {![IsSectionDirty] && $shell == $pile && [llength $OpenSeesNMResults(pile)] > 0} {
	return $OpenSeesNMResults(pile)
    }


    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set cover [expr {$sectionPropsTMP(cover,$whichSection)*$in}]
    set ductility $sectionPropsTMP(NMductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell
    set Pmax [AxialLoadCapacity]

    set coords ""


    set nu 2.0

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]

    global stopReport

    while {$nu > -1.0} {
	wipe

	update
	if {$stopReport == 1} {
	    break
	}
	
	model basic -ndm 2 -ndf 3
	
	DefineOpenSeesSection $withShell $withoutShell $pile
	
	set numIncr 100;	# Number of analysis increments
	
	set axialLoad [expr {$nu*$Pmax}]

	MomentCurvature $shell $axialLoad [expr {$ductility*$Ky}] $numIncr

	set ok 0
	set j 0
	while {$ok >= 0 && $j < $numIncr} {

	    set ok [analyze 1]
	    if {$ok < 0} {
		break
	    }
	    
	    incr j
	}
	
	set Mmax [getTime]
	
	if {$ok >= 0 && $Mmax > 0.0} {
	    lappend coords [expr {$Mmax/$ft}] $axialLoad
	}
	
	# Kill the analysis if not converged and in tension
	if {$ok < 0 && $nu < 0.0} {
	    break
	}
	
	set nu [expr {$nu - 0.05}]
    }

    if {$shell == $withShell} {
	set OpenSeesNMResults(withShell) $coords
    } 
    if {$shell == $withoutShell} {
	set OpenSeesNMResults(withoutShell) $coords
    }
    if {$shell == $pile} {
	set OpenSeesNMResults(pile) $coords
    }
    
    return $coords
}

set nmProgressOpen 0
set nmAnalysisWindowOpen 0
proc NMAnalysis {w shell} {

    global nmProgressOpen
    if {$nmProgressOpen == 1} {raise $w; return}

    global nmAnalysisWindowOpen
    if {$nmAnalysisWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Analysis Progress"

    set nmProgressOpen 1
    bind $w <Destroy> {set nmProgressOpen 0; set nmStopAnalysis 1}

    label $w.label -text "Moment-curvature analysis at"
    grid $w.label -row 0 -column 0 -sticky ew
    label $w.progress
    grid $w.progress -row 1 -column 0 -sticky ew

    global nmStopAnalysis
    set nmStopAnalysis 0

    button $w.cancel -text "Stop" -command "set nmStopAnalysis 1"
    grid $w.cancel -row 2 -column 0 -sticky ew


    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set cover [expr {$sectionPropsTMP(cover,$whichSection)*$in}]
    set ductility $sectionPropsTMP(NMductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell
    set Pmax [AxialLoadCapacity]

    set coords ""

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    set nuMax 2.0
    set nu $nuMax
    set nuMin -1.0

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]

    set Mmax 0

    while {$nu > $nuMin && $nmStopAnalysis == 0} {
	wipe

	model basic -ndm 2 -ndf 3
	
	DefineOpenSeesSection $withShell $withoutShell $pile
	
	set numIncr 100;	# Number of analysis increments
	
	set axialLoad [expr {$nu*$Pmax}]

	$w.progress config -text "N = [format %.1f $axialLoad] kip"

	update

	MomentCurvature $shell $axialLoad [expr {$ductility*$Ky}] $numIncr

	set ok 0
	set j 0
	while {$ok >= 0 && $j < $numIncr} {

	    set ok [analyze 1]
          if {$ok < 0} {
             break
          }

	    incr j
	}

	set Mx [getTime]

      if {$ok >= 0 && $Mx > 0.0} {
         lappend coords [expr {$Mx/$ft}] $axialLoad
      }

      # Kill the analysis if not converged and in tension
      if {$ok < 0 && $nu < 0.0} {
         break
      }

      if {$Mx > $Mmax} {
         set Mmax $Mx
      }

	set nu [expr {$nu - 0.05}]
    }

    global OpenSeesNMResults
    if {$shell == $withShell} {
	 set OpenSeesNMResults(withShell) $coords
    } 
    if {$shell == $withoutShell} {
	 set OpenSeesNMResults(withoutShell) $coords
    }
    if {$shell == $pile} {
	 set OpenSeesNMResults(pile) $coords
    }

    destroy $w

    createTopLevel $w "N-M Interaction"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set nmAnalysisWindowOpen 1
    bind $w <Destroy> {set nmAnalysisWindowOpen 0}

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    set Nmin [expr {$nuMin*$Pmax}]
    set Nmax [expr {$nuMax*$Pmax}]
    axesEMUGraph $graph $Mmax $Nmax 0 $Nmin
    $graph redraw

    $graph hmark 0.0 tagZero black

    if {$shell == $withShell} {
	set graphInfo(title) "Axial-Moment Interaction (CISS Section)"
    }
    if {$shell == $withoutShell} {
	set graphInfo(title) "Axial-Moment Interaction (Gap Section)"
    }
    if {$shell == $pile} {
	set graphInfo(title) "Axial-Moment Interaction (Pile Section)"
    }
    set graphInfo(xlabel) "M (kip-ft)"
    set graphInfo(ylabel) "N (kip)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

# A proc to do moment-curvature analysis of the CISS section
# input: w -- new popup window name
set momentCurvatureWindowOpen 0
proc DefineMomentCurvature {w shell} {
    global momentCurvatureWindowOpen

    if {$momentCurvatureWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Moment-Curvature Analysis"

    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The inputs define the parameters for a moment-curvature analysis with fixed axial load and target curvature ductility."

    createHelp $w $w.mbar momentCurvature text


    set momentCurvatureWindowOpen 1
    bind $w <Destroy> {set momentCurvatureWindowOpen 0}

    global sectionPropsTMP
    global columnPropsTMP
    global whichColumn
    set sectionPropsTMP(axialLoad) $columnPropsTMP(axialLoad,$whichColumn)

    frame $w.input

    label $w.input.labelLoad -text "Axial Load"
    grid $w.input.labelLoad -row 0 -column 0
    entry $w.input.axialLoad -width 5 -textvariable sectionPropsTMP(axialLoad)
    grid $w.input.axialLoad -row 0 -column 1

    label $w.input.labelduct -text "Target Ductility"
    grid $w.input.labelduct -row 1 -column 0
    entry $w.input.duct -width 5 -textvariable sectionPropsTMP(MKductility)
    grid $w.input.duct -row 1 -column 1

    button $w.input.analyze -text "Analyze" -command "MomentCurvatureAnalysis .momentCurvAnalysis $shell -123456789"
    grid $w.input.analyze -row 2 -column 0 -columnspan 2 -sticky ew

    button $w.input.close -text "Close" -command "destroy $w"
    grid $w.input.close -row 3 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top
}

set momentCurvatureAnalysisWindowOpen 0
proc MomentCurvatureAnalysis {w shell axialLoad} {

    global momentCurvatureAnalysisWindowOpen
    if {$momentCurvatureAnalysisWindowOpen == 1} {raise $w; return}

    global sectionPropsTMP
    if {$axialLoad == -123456789} {
       set axialLoad $sectionPropsTMP(axialLoad)
    }

    set coords [OpenSeesMKAnalysis $shell $axialLoad]

    createTopLevel $w "Moment-Curvature Analysis"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set momentCurvatureAnalysisWindowOpen 1
    bind $w <Destroy> {set momentCurvatureAnalysisWindowOpen 0}

    # Find max moment and curvature
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    axesEMUGraph $graph $emax $smax
    $graph redraw

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    set axialLoad [format %.1f $axialLoad]
    if {$shell == $withShell} {
       set graphInfo(title) "Moment-Curvature (CISS Section), N = $axialLoad kips"
    }
    if {$shell == $withoutShell} {
       set graphInfo(title) "Moment-Curvature (Gap Section), N = $axialLoad kips"
    }
    if {$shell == $pile} {
       set graphInfo(title) "Moment-Curvature (Pile Section), N = $axialLoad kips"
    }
    set graphInfo(xlabel) "Curvature (1/ft)"
    set graphInfo(ylabel) "Moment (kip-ft)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

proc OpenSeesMKAnalysis {shell axialLoad} {

    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set ductility $sectionPropsTMP(MKductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    wipe

    model basic -ndm 2 -ndf 3

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]
    DefineOpenSeesSection $withShell $withoutShell $pile

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]
    
    set numIncr 100;	# Number of analysis increments

    MomentCurvature $shell $axialLoad [expr {$Ky*$ductility}] $numIncr

    set coords "0 0"

    # Compute moment-curvature
    set j 0
    set ok 0
    while {$ok >= 0 && $j < $numIncr} {

	set ok [analyze 1]

	set curv   [expr {[nodeDisp 2 3]*$ft}]
	set moment [expr {[getTime]/$ft}]

	lappend coords $curv $moment
	
	incr j
    }

    return $coords
}

proc MomentCurvature {secTag axialLoad maxK {numIncr 100} } {
# Axial load: assume positive is compression
	
    # Define two nodes at (0,0)
    node 1 0.0 0.0
    node 2 0.0 0.0
    
    # Fix all degrees of freedom except axial and bending
    fix 1 1 1 1
    fix 2 0 1 0
    
    # Define element
    #                         tag ndI ndJ  secTag
    element zeroLengthSection  1   1   2  $secTag

    # Define constant axial load
    pattern Plain 1 "Constant" {
	load 2 [expr -$axialLoad] 0.0 0.0
    }

    # Define analysis parameters
    integrator LoadControl 0.0
    system UmfPack
    test NormUnbalance 1.0e-9 10 
    numberer Plain
    constraints Plain
    algorithm Newton
    analysis Static
    
    # Do one analysis for constant axial load
    analyze 1
    
    # Define reference moment
    pattern Plain 2 "Linear" {
	load 2 0.0 0.0 1.0
    }
    
    # Compute curvature increment
    set dK [expr {$maxK/$numIncr}]
    
    # Use displacement control at node 2 for section analysis
    integrator DisplacementControl 2 3 $dK 1 $dK $dK
}

proc DefaultA706 {} {
    global sectionProps
    global whichSection
    set barNum $sectionProps(ds,$whichSection)

    global rfsteelProps
    
    # Values common to all bar sizes
    set rfsteelProps(E) 29000.0
    set rfsteelProps(fye) 68.0
    set rfsteelProps(fue) 95.0
    set rfsteelProps(epsye) 0.0023

    # Onset of strain hardening
    if {$barNum >= 3 && $barNum <= 8} {set rfsteelProps(epssh) 0.015}
    if {$barNum == 9} {set rfsteelProps(epssh) 0.0125}
    if {$barNum >= 10 && $barNum <= 11} {set rfsteelProps(epssh) 0.0115}
    if {$barNum == 14} {set rfsteelProps(epssh) 0.0075}
    if {$barNum == 18} {set rfsteelProps(epssh) 0.005}
    
    # (Reduced) ultimate tensile strain
    if {$barNum <= 10} {
	set rfsteelProps(epssuR) 0.09
	set rfsteelProps(epssu) 0.12
    } else {
	set rfsteelProps(epssuR) 0.06
	set rfsteelProps(epssu) 0.09
    }
}



proc AxialLoadCapacity {} {
# Approximation of max compressive axial load
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global jacketPropsTMP
    set fy [expr {$jacketPropsTMP(fye)*$ksi}]

    set pi [expr {acos(-1.0)}]
    set Pmax [expr {$fc*$pi*0.25*$Dc*$Dc + $fy*$pi*0.25*($Dj*$Dj-$Dc*$Dc)}]

    return $Pmax
}

proc Assign_limitStates {} {
    global limitStates
    global limitStatesTMP

    foreach var [array names limitStatesTMP] {
      if {$limitStates($var) != $limitStatesTMP($var)} {
         ModelIsDirty
      }
	set limitStates($var) $limitStatesTMP($var)
    }
}

proc AssignTMP_limitStates {} {
    global limitStates
    global limitStatesTMP

    foreach var [array names limitStates] {
	set limitStatesTMP($var) $limitStates($var)
    }
}

set limitStates(steel) 0.06
set limitStates(jacket) 0.026
set limitStates(concrete) 0.02
AssignTMP_limitStates

set limitstateWindowOpen 0
proc DefineLimitStates {w} {

    global limitstateWindowOpen
    if {$limitstateWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Strain Limit States"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input values are strain levels for which, if reached during the analysis, the associated lateral displacment is shown as a vertical line on the pushover curve."

    createHelp $w $w.mbar limitStates text


    set limitstateWindowOpen 1
    bind $w <Destroy> {set limitstateWindowOpen 0}

    global limitStates
    global limitStatesTMP

    foreach var [array names limitStates] {
	set limitStatesTMP($var) $limitStates($var)
    }

    frame $w.input

    set row 0
    foreach {label tag} {"Reinforcing Steel" steel  "Steel Shell" jacket  "Concrete" concrete} {

       label $w.input.label$tag -text $label
       grid $w.input.label$tag -row $row -column 0 -sticky e
       entry $w.input.entry$tag -width 5 -textvariable limitStatesTMP($tag)
       bind $w.input.entry$tag <Return> "DefineLimitStates_OK $w"
       grid $w.input.entry$tag -row $row -column 1
       label $w.input.units$tag -text "in/in"
       grid $w.input.units$tag -row $row -column 2 -sticky w
       incr row
    }

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineLimitStates_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineLimitStates_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineLimitStates_Cancel {w} {
    AssignTMP_limitStates

    destroy $w
}

proc DefineLimitStates_OK {w} {
    
    set ok [CheckLimitStates]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_limitStates

    DefineModel [getCanvas]

    destroy $w
}

proc CheckLimitStates {} {
    global limitStatesTMP

    set bcap $limitStatesTMP(steel)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "reinforcing steel limit state"
    }

    set bcap $limitStatesTMP(jacket)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "steel shell limit state"
    }

    set bcap $limitStatesTMP(concrete)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "concrete limit state"
    }

}

proc Assign_rfsteelProps {} {
    global rfsteelProps
    global rfsteelPropsTMP

    foreach var [array names rfsteelPropsTMP] {
       if {[info exists rfsteelProps($var)] && $rfsteelProps($var) != $rfsteelPropsTMP($var)} {
          ModelIsDirty
          SectionIsDirty
       }
	 set rfsteelProps($var) $rfsteelPropsTMP($var)
    }
}

proc AssignTMP_rfsteelProps {} {
    global rfsteelProps
    global rfsteelPropsTMP

    foreach var [array names rfsteelProps] {
	set rfsteelPropsTMP($var) $rfsteelProps($var)
    }
}

set rfsteelProps(E) 29000.0
set rfsteelProps(fye) 68.0
set rfsteelProps(fue) 95.0
set rfsteelProps(C1) 1.5
set rfsteelProps(epssh) 0.0115
set rfsteelProps(epssuR) 0.06
set rfsteelProps(epssu) 0.09
AssignTMP_rfsteelProps


set rebarSteelWindowOpen 0
proc DefineRebarSteel {w} {

    global rebarSteelWindowOpen
    if {$rebarSteelWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Reinforcing Steel Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy .rebarStressStrain; destroy $w"
    bind $w <Control-Key-w> "destroy .rebarStressStrain; destroy $w"

    set text(0) "The input parameters define the stress-strain behavior of steel according to the model proposed by Raynor et al (2002)."
    set text(1) "Pressing the Stress-Strain button will produce a plot of the stress-strain behavior of the reinforcing steel."

    createHelp $w $w.mbar rfsteel text

    set rebarSteelWindowOpen 1
    bind $w <Destroy> {set rebarSteelWindowOpen 0}

    AssignTMP_rfsteelProps

    frame $w.input

    set row 0
    foreach {lbl ent unt} {"E" E ksi "f_ye" fye ksi "f_ue" fue ksi "e_sh" epssh "in/in" "e_suR" epssuR "in/in" "e_su" epssu "in/in" "C1" C1 ""} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 7 -textvariable rfsteelPropsTMP($ent)
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    button $w.input.sigeps -text "Stress-Strain" -command "RebarStressStrain .rebarStressStrain $w"
    grid $w.input.sigeps -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineRebarSteel_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineRebarSteel_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineRebarSteel_Cancel {w} {
    AssignTMP_rfsteelProps

    destroy $w
}

proc DefineRebarSteel_OK {w} {
    
    set ok [CheckRebarSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_rfsteelProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckRebarSteel {} {
    global rfsteelPropsTMP

    set E $rfsteelPropsTMP(E)
    if {![isValidDouble $E] || $E <= 0.0} {
	return E
    }

    set fye $rfsteelPropsTMP(fye)
    if {![isValidDouble $fye] || $fye < 0.0} {
	return fye
    }

    set fue $rfsteelPropsTMP(fue)
    if {![isValidDouble $fue] || $fue < 0.0} {
	return fue
    }

    set epssh $rfsteelPropsTMP(epssh)
    if {![isValidDouble $epssh] || $epssh <= 0.0} {
	return epssh
    }

    set epssuR $rfsteelPropsTMP(epssuR)
    if {![isValidDouble $epssuR] || $epssuR <= 0.0} {
	return epssuR
    }

    set epssu $rfsteelPropsTMP(epssu)
    if {![isValidDouble $epssu] || $epssu <= 0.0} {
	return epssu
    }

    set C1 $rfsteelPropsTMP(C1)
    if {![isValidDouble $C1] || $C1 <= 0.0} {
	return C1
    }
}

proc OpenSeesRebarSteel {tag} {
    global units
    set ksi $units(ksi)

    global analysisProps
    set overstrength $analysisProps(overstrength)
    set allElastic $analysisProps(allElastic)

    global rfsteelPropsTMP
    foreach var [array names rfsteelPropsTMP] {
	set $var $rfsteelPropsTMP($var)
    }
    set E [expr $E*$ksi]
    set fye [expr $fye*$ksi]
    set fue [expr $fue*$ksi]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tag $E
    } else {
	hystereticBackbone Raynor $tag $E $fye $fue $epssh $epssu $C1 [expr 0.0001*$E]
	uniaxialMaterial Backbone -$tag $tag
    }
    uniaxialMaterial Multiplier $tag -$tag $overstrength
}

proc OpenSeesRebarAnalysis {} {
    global rfsteelPropsTMP
    set epssu $rfsteelPropsTMP(epssu)
    set fye $rfsteelPropsTMP(fye)
    set fue $rfsteelPropsTMP(fue)

    global units
    set ksi $units(ksi)

    wipe

    model basic -ndm 1 -ndf 1

    node 1 0.0
    node 2 1.0

    fix 2 1

    OpenSeesRebarSteel 1

    element truss 1 1 2 1.0 1

    pattern Plain 1 Linear {
	load 1 1.0
    }

    integrator DisplacementControl 1 1 0.0001
    constraints Penalty 1.0e8 1.0e8
    test NormUnbalance 1.0e-8 10
    algorithm Newton
    numberer Plain
    system UmfPack

    analysis Static

    set coords "0 0"

    set ok 0
    set j 0
    while {$ok >= 0 && [nodeDisp 1 1] < [expr $epssu]} {

	set ok [analyze 1]

	set strain [nodeDisp 1 1]
	set stress [expr {[getTime]/$ksi}]
	lappend coords $strain $stress

	incr j
    }

    return $coords
}

set rebarStressStrainWindowOpen 0
proc RebarStressStrain {w p} {

    set ok [CheckRebarSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global rebarStressStrainWindowOpen
    if {$rebarStressStrainWindowOpen == 1} {return}

    createTopLevel $w "Reinforcing Steel Stress-Strain"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    set coords [OpenSeesRebarAnalysis]
    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    # Find max stress and strain
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    set graphInfo(title) "Reinforcing Steel Stress-Strain"
    set graphInfo(xlabel) "Strain (in/in)"
    set graphInfo(ylabel) "Stress (ksi)"
    labelEMUGraph $c $graph graphInfo $width $height


    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set rebarStressStrainWindowOpen 1
    bind $w <Destroy> {set rebarStressStrainWindowOpen 0}

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

proc Assign_jacketProps {} {
    global jacketProps
    global jacketPropsTMP

    foreach var [array names jacketPropsTMP] {
       if {[info exists jacketProps($var)] && $jacketProps($var) != $jacketPropsTMP($var)} {
          ModelIsDirty
          SectionIsDirty
       }
	set jacketProps($var) $jacketPropsTMP($var)
    }
}

proc AssignTMP_jacketProps {} {
    global jacketProps
    global jacketPropsTMP

    foreach var [array names jacketProps] {
	set jacketPropsTMP($var) $jacketProps($var)
    }
}

set jacketProps(E) 29000.0
set jacketProps(fye) 60.0
set jacketProps(fue) 78.0
set jacketProps(C1) 1.5
set jacketProps(epssh) 0.0115
set jacketProps(epssuR) 0.04
set jacketProps(epssu) 0.06
AssignTMP_jacketProps


set jacketSteelWindowOpen 0
proc DefineJacketSteel {w} {

    global jacketSteelWindowOpen
    if {$jacketSteelWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Steel Shell Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy .jacketStressStrain; destroy $w"
    bind $w <Control-Key-w> "destroy .jacketStressStrain; destroy $w"

    set text(0) "The input parameters define the stress-strain behavior of steel according to the model proposed by Raynor et al (2002)."
    set text(1) "Pressing the Stress-Strain button will produce a plot of the stress-strain behavior of the steel shell."

    createHelp $w $w.mbar jacketSteel text


    set jacketSteelWindowOpen 1
    bind $w <Destroy> {set jacketSteelWindowOpen 0}

    AssignTMP_jacketProps

    frame $w.input

    set row 0
    foreach {lbl ent unt} {"E" E ksi "f_ye" fye ksi "f_ue" fue ksi "e_sh" epssh "in/in" "e_suR" epssuR "in/in" "e_su" epssu "in/in" "C1" C1 ""} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 7 -textvariable jacketPropsTMP($ent)
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    button $w.input.sigeps -text "Stress-Strain" -command "JacketStressStrain .jacketStressStrain $w"
    grid $w.input.sigeps -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineJacketSteel_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineJacketSteel_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineJacketSteel_Cancel {w} {
    AssignTMP_jacketProps

    destroy $w
}

proc DefineJacketSteel_OK {w} {

    set ok [CheckJacketSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_jacketProps

    DefineModel .fBent.canvas

    destroy $w
}

proc CheckJacketSteel {} {
    global jacketPropsTMP

    set E $jacketPropsTMP(E)
    if {![isValidDouble $E] || $E <= 0.0} {
	return E
    }

    set fye $jacketPropsTMP(fye)
    if {![isValidDouble $fye] || $fye < 0.0} {
	return fye
    }

    set fue $jacketPropsTMP(fue)
    if {![isValidDouble $fue] || $fue < 0.0} {
	return fue
    }

    set epssh $jacketPropsTMP(epssh)
    if {![isValidDouble $epssh] || $epssh <= 0.0} {
	return epssh
    }

    set epssuR $jacketPropsTMP(epssuR)
    if {![isValidDouble $epssuR] || $epssuR <= 0.0} {
	return epssuR
    }

    set epssu $jacketPropsTMP(epssu)
    if {![isValidDouble $epssu] || $epssu <= 0.0} {
	return epssu
    }

    set C1 $jacketPropsTMP(C1)
    if {![isValidDouble $C1] || $C1 <= 0.0} {
	return C1
    }
}

proc OpenSeesJacketSteel {tag} {
    global units
    set ksi $units(ksi)

    global analysisProps
    set overstrength $analysisProps(overstrength)
    set allElastic $analysisProps(allElastic)

    global jacketPropsTMP
    foreach var [array names jacketPropsTMP] {
	set $var $jacketPropsTMP($var)
    }
    set E [expr $E*$ksi]
    set fye [expr $fye*$ksi]
    set fue [expr $fue*$ksi]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tag $E
    } else {
	hystereticBackbone Raynor $tag $E $fye $fue $epssh $epssu $C1 [expr 0.0001*$E]
	uniaxialMaterial Backbone -$tag $tag
    }
    uniaxialMaterial Multiplier $tag -$tag $overstrength
}

proc OpenSeesJacketAnalysis {} {
    global units
    set ksi $units(ksi)

    global jacketPropsTMP
    set epssu $jacketPropsTMP(epssu)
    set fye $jacketPropsTMP(fye)
    set fue $jacketPropsTMP(fue)

    wipe

    model basic -ndm 1 -ndf 1

    node 1 0.0
    node 2 1.0

    fix 2 1

    OpenSeesJacketSteel 1

    element truss 1 1 2 1.0 1

    pattern Plain 1 Linear {
	load 1 1.0
    }

    integrator DisplacementControl 1 1 0.0001
    constraints Penalty 1.0e8 1.0e8
    test NormUnbalance 1.0e-8 10
    algorithm Newton
    numberer Plain
    system UmfPack

    analysis Static

    set coords "0 0"

    set ok 0
    set j 0
    while {$ok >= 0 && [nodeDisp 1 1] < $epssu} {

	set ok [analyze 1]

	set strain [nodeDisp 1 1]
	set stress [expr {[getTime]/$ksi}]
	lappend coords $strain $stress

	incr j
    }

    return $coords
}

set jacketStressStrainWindowOpen 0
proc JacketStressStrain {w p} {

    set ok [CheckJacketSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global jacketStressStrainWindowOpen
    if {$jacketStressStrainWindowOpen == 1} {raise $w; return}

    set coords [OpenSeesJacketAnalysis]

    createTopLevel $w "Steel Shell Stress-Strain"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set jacketStressStrainWindowOpen 1
    bind $w <Destroy> {set jacketStressStrainWindowOpen 0}

    # Find max stress and strain
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    axesEMUGraph $graph $emax $smax
    $graph redraw

    set graphInfo(title) "Steel Shell Stress-Strain"
    set graphInfo(xlabel) "Strain (in/in)"
    set graphInfo(ylabel) "Stress (ksi)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

proc Assign_concreteProps {} {
    global concreteProps
    global concretePropsTMP

    foreach var [array names concretePropsTMP] {
      if {[info exists concreteProps($var)] && $concreteProps($var) != $concretePropsTMP($var)} {
         ModelIsDirty
         SectionIsDirty
      }
	set concreteProps($var) $concretePropsTMP($var)
    }
}

proc AssignTMP_concreteProps {} {
    global concreteProps
    global concretePropsTMP

    foreach var [array names concreteProps] {
	set concretePropsTMP($var) $concreteProps($var)
    }
}

set concreteProps(fc) 5.2
set concreteProps(eco) 0.002
set concreteProps(esp) 0.006
AssignTMP_concreteProps

proc ManderModel {whichSection} {
    global units
    set ksi $units(ksi)
    set in $units(in)

    global concretePropsTMP

    set fc $concretePropsTMP(fc)
    if {![isValidDouble $fc] || $fc == 0.0} {return}
    set fc [expr {$fc*$ksi}]

    global sectionPropsTMP

    set Dj $sectionPropsTMP(Dj,$whichSection)
    if {![isValidDouble $Dj]} {return}
    set Dj [expr {$Dj*$in}]

    set tj $sectionPropsTMP(tj,$whichSection)
    if {![isValidDouble $tj]} {return}
    set tj [expr {$tj*$in}]

    global jacketPropsTMP
    set fyj $jacketPropsTMP(fye)
    set fyj [expr {$fyj*$ksi}]

    # Inside diameter of steel shell
    set Dc [expr {$Dj-2*$tj}]

    # Horizontal volumetric confining ratio of steel shell, Eq. 3.4
    set rhosj [expr {4*$tj/$Dc}]

    # Radial confining stress of steel shell, Eq. 3.3
    set fl [expr {0.5*$fyj*$rhosj}]
    
    # Confined concrete compressive strength
    set fcc [expr {$fc*(2.254*sqrt(1+7.94*$fl/$fc) - 2*$fl/$fc - 1.254)}]

    set concretePropsTMP(ecc,$whichSection) [expr {0.002*(1+5*($fcc/$fc-1))}]
    set concretePropsTMP(fcc,$whichSection) [expr {$fcc/$ksi}]


    set s $sectionPropsTMP(sphoop,$whichSection)
    if {![isValidDouble $s]} {return}
    set s [expr {$s*$in}]

    set fy $sectionPropsTMP(fyhoop,$whichSection)
    if {![isValidDouble $fy]} {return}
    set fy [expr {$fy*$ksi}]

    set cover $sectionPropsTMP(cover,$whichSection)
    if {![isValidDouble $cover]} {return}
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(dshoop,$whichSection)
    set Asp [BarArea $ds]
    set hoopDiam [BarDiam $ds]

    set sp [expr {$s-$hoopDiam}]

    set Nbars $sectionPropsTMP(Nbars,$whichSection)
    set dds $sectionPropsTMP(ds,$whichSection)
    set Adds [BarArea $dds]

    # Diameter of core section
    set Dcore [expr {$Dc-2*$cover}]
    set pi [expr {2*asin(1.0)}]
    set Acore [expr {$pi*0.25*$Dcore*$Dcore}]

    set rhocc [expr {$Nbars*$Adds/$Acore}]

    set ds [expr {$Dcore-$hoopDiam}]
    set ke [expr {(1-$sp/(2*$ds))/(1-$rhocc)}]

    set fl [expr {0.5*$ke*4*$Asp/($ds*$s)*$fy}]

    # Confined concrete compressive strength
    set fcc [expr {$fc*(2.254*sqrt(1+7.94*$fl/$fc) - 2*$fl/$fc - 1.254)}]

    set concretePropsTMP(ecchoop,$whichSection) [expr {0.002*(1+5*($fcc/$fc-1))}]
    set concretePropsTMP(fcchoop,$whichSection) [expr {$fcc/$ksi}]

    # If there is no steel shell, then set the section concrete props to be the same as the gap region
    if {$tj <= 0.0} {
	set concretePropsTMP(ecc,$whichSection) $concretePropsTMP(ecchoop,$whichSection)
	set concretePropsTMP(fcc,$whichSection) $concretePropsTMP(fcchoop,$whichSection)
    }
}

ManderModel 1
ManderModel 2
Assign_concreteProps

set concreteWindowOpen 0
proc DefineConcrete {w} {

    global concreteWindowOpen
    if {$concreteWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Concrete Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy .concreteStressStrain; destroy $w"
    bind $w <Control-Key-w> "destroy .concreteStressStrain; destroy $w"

    set text(0) "The first three inputs are nominal values for unconfined concrete: \"f_ce\" is the expected concrete strength; \"e_co\" is the strain at which f_c is reached; and \"e_sp\" is the strain at which spalling occurs."
    set text(1) "Updating these values will cause the program to compute \"f_cc\" and \"e_cc\", the peak compressive stress and strain, respectively, according to Mander-based CISS procedure of Chai et al (1994).  The parameters \"f_cc\" and \"e_cc\" can be overwritten if desired."
    set text(2) "Pressing the Stress-Strain button will produce a plot of the stress-strain behavior of the confined and unconfined concrete."

    createHelp $w $w.mbar concrete text


    set concreteWindowOpen 1
    bind $w <Destroy> {set concreteWindowOpen 0}

    AssignTMP_concreteProps

    frame $w.input

    ManderModel 1
    ManderModel 2

    set row 0
    foreach {lbl ent unt} {"Unconfined: f_ce" fc ksi "e_co" eco "in/in" "e_sp" esp "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent)
	bind $w.input.entry$row <KeyRelease> "ManderModel 1; ManderModel 2"
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    #button $w.input.mander -text "Mander Model" -command "ManderModel"
    #grid $w.input.mander -column 0 -row $row -columnspan 2 -sticky ew
    incr row

    foreach {lbl ent unt} {"Gap Section: f_cc" fcchoop ksi "e_cc" ecchoop "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,1) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    incr row

    foreach {lbl ent unt} {"CISS Section: f_cc" fcc ksi "e_cc" ecc "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,1) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    incr row

    foreach {lbl ent unt} {"Pile Section: f_cc" fcc ksi "e_cc" ecc "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,2) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    button $w.input.sigeps -text "Stress-Strain" -command "ManderModel 1; ManderModel 2; ConcreteStressStrain .concreteStressStrain $w"
    grid $w.input.sigeps -column 0 -row $row -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineConcrete_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineConcrete_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineConcrete_Cancel {w} {
    AssignTMP_concreteProps

    destroy $w
}

proc DefineConcrete_OK {w} {

    set ok [CheckConcrete]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_concreteProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckConcrete {} {
    global concretePropsTMP

    set fc $concretePropsTMP(fc)
    if {![isValidDouble $fc] || $fc == 0.0} {
	return fc
    }

    set eco $concretePropsTMP(eco)
    if {![isValidDouble $eco] || $eco == 0.0} {
	return eco
    }

    set esp $concretePropsTMP(esp)
    if {![isValidDouble $esp] || $esp == 0.0} {
	return esp
    }
}

proc OpenSeesConcrete {tagUnconf tagCISS tagHoop tagPile} {
    global units
    set ksi $units(ksi)
    set psi $units(psi)
    
    global concretePropsTMP
    set fc $concretePropsTMP(fc)
    set fc [expr {$fc*$ksi}]

    set eco $concretePropsTMP(eco)
    set esp $concretePropsTMP(esp)

    global analysisProps
    set overstrength $analysisProps(overstrength)
    set allElastic $analysisProps(allElastic)

    set Ec [expr {57*sqrt($fc/$psi)*$ksi}]
    #set Ec [expr {2*$fc/$eco}]
    
    if {$allElastic} {
	uniaxialMaterial Elastic -$tagUnconf $Ec
    } else {
	uniaxialMaterial Concrete01 -$tagUnconf $fc $eco 0.0 $esp
    }
    uniaxialMaterial Multiplier $tagUnconf -$tagUnconf $overstrength

    # Force recalc of confined props to ensure
    # they are used in all analyses
    ManderModel 1

    set ecc $concretePropsTMP(ecc,1)
    set fcc $concretePropsTMP(fcc,1)
    set fcc [expr {$fcc*$ksi}]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tagCISS $Ec
    } else {
	hystereticBackbone Mander $tagCISS $fcc $ecc $Ec
	uniaxialMaterial Backbone -$tagCISS $tagCISS
    }
    uniaxialMaterial Multiplier $tagCISS -$tagCISS $overstrength

    set ecchoop $concretePropsTMP(ecchoop,1)
    set fcchoop $concretePropsTMP(fcchoop,1)
    set fcchoop [expr {$fcchoop*$ksi}]
   
    if {$allElastic} {
	uniaxialMaterial Elastic -$tagHoop $Ec
    } else {
	hystereticBackbone Mander $tagHoop $fcchoop $ecchoop $Ec
	uniaxialMaterial Backbone -$tagHoop $tagHoop
    }
    uniaxialMaterial Multiplier $tagHoop -$tagHoop $overstrength

    ManderModel 2

    set ecc $concretePropsTMP(ecc,2)
    set fcc $concretePropsTMP(fcc,2)
    set fcc [expr {$fcc*$ksi}]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tagPile $Ec
    } else {
	hystereticBackbone Mander $tagPile $fcc $ecc $Ec
	uniaxialMaterial Backbone -$tagPile $tagPile
    }
    uniaxialMaterial Multiplier $tagPile -$tagPile $overstrength
}

proc OpenSeesConcreteAnalysis {type} {
    global units
    set ksi $units(ksi)
    set psi $units(psi)

    global concretePropsTMP
    foreach var [array names concretePropsTMP] {
	set $var $concretePropsTMP($var)
    }
    set ec $concretePropsTMP(eco)
    set fc $concretePropsTMP(fc)
    set fc [expr {$fc*$ksi}]

    wipe

    model basic -ndm 1 -ndf 1

    node 1 0.0
    node 2 1.0

    fix 2 1

    OpenSeesConcrete 1 2 3 4

    set matTag 1; set strainLimit $esp
    if {$type == "CISS"} {
	set ecc $concretePropsTMP(ecc,1)
        set matTag 2; set strainLimit [expr {2*$ecc}]
    }
    if {$type == "Hoop"} {
	set ecc $concretePropsTMP(ecchoop,1)
        set matTag 3; set strainLimit [expr {2*$ecc}]
    }
    if {$type == "Pile"} {
	set ecc $concretePropsTMP(ecc,2)
        set matTag 4; set strainLimit [expr {2*$ecc}]
    }

    element truss 1 1 2 1.0 $matTag

    uniaxialMaterial Elastic 5 [expr {0.001*$fc/$ec}]
    element truss 2 1 2 1.0 5

    pattern Plain 1 Linear {
	load 1 1.0
    }

    integrator DisplacementControl 1 1 0.0001
    constraints Plain
    numberer Plain
    test NormUnbalance 1.0e-8 10
    algorithm Newton
    system UmfPack

    analysis Static

    set coords "0 0"

    set ok 0
    set j 0
    while {$ok >= 0 && [nodeDisp 1 1] < $strainLimit} {

	set ok [analyze 1]

	set strain [nodeDisp 1 1]
	set stress [expr {[getTime]/$ksi}]
	lappend coords $strain $stress

	incr j
    }

    return $coords
}

set concreteStressStrainWindowOpen 0
proc ConcreteStressStrain {w p} {

    set ok [CheckConcrete]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global concreteStressStrainWindowOpen
    if {$concreteStressStrainWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Concrete Stress-Strain"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    set emax 0
    set smax 0

    set coords1 [OpenSeesConcreteAnalysis CISS]
    $graph data d1 -colour blue -points 0 -lines 1 -coords $coords1

    set coords2 [OpenSeesConcreteAnalysis Unconfined]
    $graph data d2 -colour red -points 0 -lines 1 -coords $coords2

    set coords3 [OpenSeesConcreteAnalysis Hoop]
    $graph data d3 -colour black -points 0 -lines 1 -coords $coords3

    set coords4 [OpenSeesConcreteAnalysis Pile]
    $graph data d4 -colour black -points 0 -lines 1 -coords $coords4

    foreach {e s} "$coords1 $coords2 $coords3 $coords4" {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    global sectionPropsTMP
    set pileSameAsColumn $sectionPropsTMP(pileSameAsColumn)

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add cascade -label "Export" -menu $m.concretes
    set m2 [menu $m.concretes]
    $m2 add command -label "Unconfined" -command "ExportData [list $coords2] $w"
    $m2 add command -label "Gap" -command "ExportData [list $coords3] $w"
    if {$pileSameAsColumn} {
	$m2 add command -label "CISS/Pile" -command "ExportData [list $coords1] $w"
    } else {
	$m2 add command -label "CISS" -command "ExportData [list $coords1] $w"	
	$m2 add command -label "Pile" -command "ExportData [list $coords4] $w"
    }
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set concreteStressStrainWindowOpen 1
    bind $w <Destroy> {set concreteStressStrainWindowOpen 0}


    global concretePropsTMP

    if {$pileSameAsColumn} {
	set xC [lindex $coords1 end-1]
	set yC [lindex $coords1 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	    -text "CISS/Pile" -anchor ne -tag ciss
	$c bind ciss <Double-Button-1> "ExportData [list $coords1] $w"
    } else {
	set xC [lindex $coords1 end-1]
	set yC [lindex $coords1 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	    -text "CISS" -anchor ne -tag ciss
	$c bind ciss <Double-Button-1> "ExportData [list $coords1] $w"

	set xC [lindex $coords4 end-1]
	set yC [lindex $coords4 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	    -text "Pile" -anchor ne -tag pile
	$c bind pile <Double-Button-1> "ExportData [list $coords4] $w"
    }

    set xC [lindex $coords2 end-1]
    set yC [lindex $coords2 end]
    $c create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Unconfined" -anchor sw -tag unconfined
    $c bind unconfined <Double-Button-1> "ExportData [list $coords2] $w"
    
    set xC [lindex $coords3 end-1]
    set yC [lindex $coords3 end]
    $c create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Gap" -anchor ne -tag hoop
    $c bind hoop <Double-Button-1> "ExportData [list $coords3] $w"

    set graphInfo(title) "Concrete Stress-Strain"
    set graphInfo(xlabel) "Strain (in/in)"
    set graphInfo(ylabel) "Stress (ksi)"
    labelEMUGraph $c $graph graphInfo $width $height


    frame $w.output

    text $w.output.text -width 15
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    $w.output.text insert end "Use the File->Export menu or double-click on a label, e.g., CISS, to export data"

    $w.output.text configure -state disabled


}

proc Assign_columnProps {} {
    global units
    set in $units(in)

    global sectionPropsTMP
    set Dj [expr $sectionPropsTMP(Dj,1)*$in]

    global columnProps
    global columnPropsTMP

    # Check that columns are not overlapping
    for {set i 2} {$i <= $columnPropsTMP(N)} {incr i} {
	if {$columnPropsTMP(space,$i) < $Dj} {
	    set columnPropsTMP(space,$i) $Dj
	}
    }

    # Make all column spacings equal as per user input
    global spaceAllColumns
    global whichColumn
    if {$spaceAllColumns != 0} {
       set space $columnPropsTMP(space,$whichColumn)
       for {set i 2} {$i <= $columnPropsTMP(N)} {incr i} {
          set columnPropsTMP(space,$i) $space
       }
    }
    set spaceAllColumns 0

    # Make all gravity loads the same as per user input
    global sameLoadAllColumns
    if {$sameLoadAllColumns != 0} {
       set load $columnPropsTMP(axialLoad,$whichColumn)
       set pmultT $columnPropsTMP(pMultT,$whichColumn)
       set pmultL $columnPropsTMP(pMultL,$whichColumn)
       for {set i 1} {$i <= $columnPropsTMP(N)} {incr i} {
          set columnPropsTMP(axialLoad,$i) $load
          set columnPropsTMP(pMultT,$i) $pmultT
          set columnPropsTMP(pMultL,$i) $pmultL
       }
    }
    set sameLoadAllColumns 0

    foreach var [array names columnPropsTMP] {
      if {[info exists columnProps($var)] && $columnProps($var) != $columnPropsTMP($var)} {
         ModelIsDirty
      }
	set columnProps($var) $columnPropsTMP($var)
    }
}

proc AssignTMP_columnProps {} {
    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
      set columnPropsTMP($var) $columnProps($var)
    }
}

set columnProps(L) 14.0
set columnProps(N) 3
set axialLoad [expr 0.1*[AxialLoadCapacity]]
set axialLoad [format %.1f $axialLoad]
for {set i 1} {$i <= $columnProps(N)} {incr i} {
    set columnProps(axialLoad,$i) $axialLoad
    set columnProps(pMultT,$i) 1.0
    set columnProps(pMultL,$i) 1.0
}
set columnProps(space,1) 0.0
for {set i 2} {$i <= $columnProps(N)} {incr i} {
    set columnProps(space,$i) 8.0
}
AssignTMP_columnProps

proc ChangeColumns {} {
    global columnProps
    global columnPropsTMP
    
    if {$columnPropsTMP(N) <= 0} {
	set columnPropsTMP(N) 1
    }

    # Assign default values if there are new columns
    set N $columnProps(N)
    set lastSpace $columnProps(space,$N)
    set lastAxialLoad $columnProps(axialLoad,$N)
    set lastPMultT $columnProps(pMultT,$N)
    set lastPMultL $columnProps(pMultL,$N)
    for {set i [expr $columnProps(N)+1]} {$i <= $columnPropsTMP(N)} {incr i} {
	set columnPropsTMP(space,$i) $lastSpace
	set columnPropsTMP(axialLoad,$i) $lastAxialLoad
	set columnPropsTMP(pMultT,$i) $lastPMultT
	set columnPropsTMP(pMultL,$i) $lastPMultL
    }
}

proc ChangeGravityLoadMenu {m} {
    global columnProps
    global columnPropsTMP

    for {set i [expr $columnProps(N)+1]} {$i <= $columnPropsTMP(N)} {incr i} {
	$m add command -label "Column/Pile $i" -command "set whichColumn $i; DefineGravityLoad .gravity"
    }
    for {set i $columnProps(N)} {$i > $columnPropsTMP(N)} {incr i -1} {
	$m delete $i
    }
}

set spaceAllColumns 0
set columnSpacingWindowOpen 0
proc ColumnSpacing {w} {

    global columnSpacingWindowOpen
    if {$columnSpacingWindowOpen == 1} {raise $w; return}

    global whichColumn
    if {$whichColumn == 1} {return}

    createTopLevel $w "Column Spacing"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the center-to-center distance between the designated columns."

    createHelp $w $w.mbar columnSpace text


    set columnSpacingWindowOpen 1
    bind $w <Destroy> {set columnSpacingWindowOpen 0}

    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
	set columnPropsTMP($var) $columnProps($var)
    }

    frame $w.input

    global spaceAllColumns
    set spaceAllColumns 0

    label $w.input.spacing -text "Spacing between columns [expr $whichColumn-1] and $whichColumn"
    grid $w.input.spacing -row 0 -column 0 -columnspan 2
    entry $w.input.entry -textvariable columnPropsTMP(space,$whichColumn) -width 5
    grid $w.input.entry -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w
    checkbutton $w.input.all -text "Apply to all" -variable spaceAllColumns
    grid $w.input.all -row 2 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSpacing_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSpacing_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineSpacing_Cancel {w} {
    AssignTMP_columnProps

    destroy $w
}

proc DefineSpacing_OK {w} {
    
    set ok [CheckSpacing]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_columnProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSpacing {} {
    global columnPropsTMP
    set N $columnPropsTMP(N)

    for {set i 2} {$i <= $N} {incr i} {
       set load $columnPropsTMP(space,$i)
       if {![isValidDouble $load] || $load <= 0.0} {
          return "spacing between columns [expr $i-1] and $i"
       }
    }
}


set sameLoadAllColumns 0
set gravityLoadWindowOpen 0
proc DefineGravityLoad {w} {

    global gravityLoadWindowOpen
    if {$gravityLoadWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Column/Pile Loads"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the gravity load applied to the indicated column and held constant during the pushover analysis."
    set text(1) "Positive values indicate compression while negative values indicate tension."

    createHelp $w $w.mbar gravityLoad text


    set gravityLoadWindowOpen 1
    bind $w <Destroy> {set gravityLoadWindowOpen 0}
    
    global whichColumn

    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
	set columnPropsTMP($var) $columnProps($var)
    }

    frame $w.input

    global sameLoadAllColumns
    set sameLoadAllColumns 0

    set row 0

    label $w.input.col -text "Column/Pile $whichColumn" 
    grid $w.input.col -row $row -column 0 -columnspan 3

    incr row

    label $w.input.loadLabel -text "Gravity Load"
    grid $w.input.loadLabel -row $row -column 0 -sticky e
    entry $w.input.load -width 5 -textvariable columnPropsTMP(axialLoad,$whichColumn)
    bind $w.input.load <Return> "DefineGravityLoad_OK $w"
    grid $w.input.load -row $row -column 1 -sticky e
    label $w.input.ft -text "kip"
    grid $w.input.ft -row $row -column 2 -sticky w

    incr row

    label $w.input.pmultLabelT -text "p-Multiplier Transverse"
    grid $w.input.pmultLabelT -row $row -column 0 -sticky e
    entry $w.input.pmultT -width 5 -textvariable columnPropsTMP(pMultT,$whichColumn)
    bind $w.input.pmultT <Return> "DefineGravityLoad_OK $w"
    grid $w.input.pmultT -row $row -column 1 -sticky e

    incr row

    label $w.input.pmultLabelL -text "p-Multiplier Longitudinal"
    grid $w.input.pmultLabelL -row $row -column 0 -sticky e
    entry $w.input.pmultL -width 5 -textvariable columnPropsTMP(pMultL,$whichColumn)
    bind $w.input.pmultL <Return> "DefineGravityLoad_OK $w"
    grid $w.input.pmultL -row $row -column 1 -sticky e

    incr row

    checkbutton $w.input.all -text "Apply to all" -variable sameLoadAllColumns
    grid $w.input.all -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineGravityLoad_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineGravityLoad_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineGravityLoad_Cancel {w} {
    AssignTMP_columnProps

    destroy $w
}

proc DefineGravityLoad_OK {w} {
    
    set ok [CheckGravityLoad]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_columnProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckGravityLoad {} {
    global columnPropsTMP
    set N $columnPropsTMP(N)

    for {set i 1} {$i <= $N} {incr i} {
       set load $columnPropsTMP(axialLoad,$i)
       if {![isValidDouble $load]} {
          return "gravity load on column $i"
       }
       set load $columnPropsTMP(pMultT,$i)
       if {![isValidDouble $load] || $load < 0.0} {
          return "p-multiplier transverse on pile $i"
       }
       set load $columnPropsTMP(pMultL,$i)
       if {![isValidDouble $load] || $load < 0.0} {
          return "p-multiplier longitudinal on pile $i"
       }
    }
}

proc SetAxialLoad {} {
    global whichColumn

    global columnProps
    set columnProps(axialLoad,$whichColumn) $columnProps(axialLoad,0)
}

proc Assign_hingeProps {} {
    global hingeProps
    global hingePropsTMP

    foreach var [array names hingePropsTMP] {
      if {[info exists hingeProps($var)] && $hingeProps($var) != $hingePropsTMP($var)} {
         ModelIsDirty
      }
	set hingeProps($var) $hingePropsTMP($var)
    }
}

proc AssignTMP_hingeProps {} {
    global hingeProps
    global hingePropsTMP

    foreach var [array names hingeProps] {
	set hingePropsTMP($var) $hingeProps($var)
    }
}

set hingeProps(Gf) 2.0
set hingeProps(coverConfined) 1
set hingeProps(coreConfined) 0
AssignTMP_hingeProps

set hingeWindowOpen 0
proc DefineHinge {w} {
    global hingeWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$hingeWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Gap Region"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The first input value, \"Gf\" is the gap between the bent cap and the steel jacket."
    set text(1) "Changing the value of Gf will cause the program to automatically compute the plastic hinge length from the input gap value and the reinforcing steel properties according to Eq. 4.11.6-3 in design specification. This plastic hinge length can be overwritten if desired."
    set text(2) "Indicate whether the concrete in the gap region should be modelled as confined or unconfined."

    createHelp $w $w.mbar hinge text


    set hingeWindowOpen 1
    bind $w <Destroy> {set hingeWindowOpen 0}

    AssignTMP_hingeProps

    frame $w.input

    label $w.input.labelGf -text "Gap, Gf"
    grid $w.input.labelGf -row 0 -column 0 -sticky e
    entry $w.input.entryGf -width 5 -textvariable hingePropsTMP(Gf)
    bind $w.input.entryGf <KeyRelease> "Computelp"
    bind $w.input.entryGf <Return> "DefineHinge_OK $w"
    grid $w.input.entryGf -row 0 -column 1
    label $w.input.unitGf -text "in"
    grid $w.input.unitGf -row 0 -column 2 -sticky w

    label $w.input.labellp -text "Plastic Hinge Length, lp"
    grid $w.input.labellp -row 1 -column 0 -sticky e
    entry $w.input.entrylp -width 5 -textvariable hingePropsTMP(lp)
    bind $w.input.entrylp <Return> "DefineHinge_OK $w"
    grid $w.input.entrylp -row 1 -column 1
    label $w.input.unitlp -text "ft"
    grid $w.input.unitlp -row 1 -column 2 -sticky w

    pack $w.input -side top

    frame $w.confine

    label $w.confine.radioCore -text "Core Confinement"
    grid $w.confine.radioCore -row 0 -column 0 -sticky e
    radiobutton $w.confine.coreYes -variable hingePropsTMP(coreConfined) -text "Steel Shell" -value 1
    grid $w.confine.coreYes -row 0 -column 1
    radiobutton $w.confine.coreNo -variable hingePropsTMP(coreConfined) -text "Spiral Hoop" -value 0 -command ""
    grid $w.confine.coreNo -row 0 -column 2

    label $w.confine.radioGap -text "Cover Confinement"
    grid $w.confine.radioGap -row 1 -column 0 -sticky e
    radiobutton $w.confine.gapYes -variable hingePropsTMP(coverConfined) -text "Same as Core" -value 1
    grid $w.confine.gapYes -row 1 -column 1
    radiobutton $w.confine.gapNo -variable hingePropsTMP(coverConfined) -text "Unconfined" -value 0
    grid $w.confine.gapNo -row 1 -column 2

    pack $w.confine -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineHinge_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineHinge_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineHinge_Cancel {w} {
    AssignTMP_hingeProps

    destroy $w
}

proc DefineHinge_OK {w} {
    
    set ok [CheckHinge]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_hingeProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckHinge {} {
    global hingePropsTMP

    set Gf $hingePropsTMP(Gf)
    if {![isValidDouble $Gf] || $Gf < 0.0} {
	return Gf
    }

    set lp $hingePropsTMP(lp)
    if {![isValidDouble $lp] || $lp < 0.0} {
	return lp
    }
}


proc Computelp {} {
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP
    set ds $sectionPropsTMP(ds,1)
    if {![isValidDouble $ds]} {return}

    global rfsteelPropsTMP
    set fye $rfsteelPropsTMP(fye)
    if {![isValidDouble $fye]} {return}

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    if {![isValidDouble $Gf]} {return}

    set lp [expr {$Gf*$in + 0.3*$fye*[BarDiam $ds]}]

    # Make sure computed lp is less than the column above grade height
    global columnPropsTMP
    set L $columnPropsTMP(L)
    if {$lp > $L} {
	set lp [expr {0.2*$L}]
    }

    set hingePropsTMP(lp) [format %.2f $lp]
}

# Initialize plastic hinge length
Computelp
set hingeProps(lp) $hingePropsTMP(lp)

proc Assign_bentcapProps {} {
    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapPropsTMP] {
       if {[info exists bentcapProps($var)] && $bentcapProps($var) != $bentcapPropsTMP($var)} {
          ModelIsDirty
       }
       set bentcapProps($var) $bentcapPropsTMP($var)
    }

    foreach var [array names bentcapPropsTMP] {
	set bentcapProps($var) $bentcapPropsTMP($var)
    }
}

proc AssignTMP_bentcapProps {} {
    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapProps] {
	set bentcapPropsTMP($var) $bentcapProps($var)
    }
}

set bentcapProps(dcap) 48.0
set bentcapProps(bcap) 72.0
set bentcapProps(leftoverhang) 18.0
set bentcapProps(rightoverhang) 18.0
AssignTMP_bentcapProps

set bentcapWindowOpen 0
proc DefineBentcap {w} {

    global bentcapWindowOpen
    if {$bentcapWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Bent Cap"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input values are used to determine the bent cap stiffness during the pushover analysis in lieu of rigid links."
    set text(1) "Changes in the depth are shown on screen, while changes in the width are not visible since this dimension is perpendicular to the plane of view."

    createHelp $w $w.mbar bentcap text


    set bentcapWindowOpen 1
    bind $w <Destroy> {set bentcapWindowOpen 0}

    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapProps] {
	set bentcapPropsTMP($var) $bentcapProps($var)
    }

    frame $w.input

    set row 0

    label $w.input.labeld -text "Depth"
    grid $w.input.labeld -row $row -column 0 -sticky e
    entry $w.input.entryd -width 5 -textvariable bentcapPropsTMP(dcap)
    bind $w.input.entryd <Return> "DefineBentcap_OK $w"
    grid $w.input.entryd -row $row -column 1
    label $w.input.unitd -text "in"
    grid $w.input.unitd -row $row -column 2 -sticky w

    incr row

    label $w.input.labelb -text "Width"
    grid $w.input.labelb -row $row -column 0 -sticky e
    entry $w.input.entryb -width 5 -textvariable bentcapPropsTMP(bcap)
    bind $w.input.entryb <Return> "DefineBentcap_OK $w"
    grid $w.input.entryb -row $row -column 1
    label $w.input.unitb -text "in"
    grid $w.input.unitb -row $row -column 2 -sticky w

    incr row

    label $w.input.labello -text "Left Overhang"
    grid $w.input.labello -row $row -column 0 -sticky e
    entry $w.input.entrylo -width 5 -textvariable bentcapPropsTMP(leftoverhang)
    bind $w.input.entrylo <Return> "DefineBentcap_OK $w"
    grid $w.input.entrylo -row $row -column 1
    label $w.input.unitlo -text "in"
    grid $w.input.unitlo -row $row -column 2 -sticky w

    incr row

    label $w.input.labelro -text "Right Overhang"
    grid $w.input.labelro -row $row -column 0 -sticky e
    entry $w.input.entryro -width 5 -textvariable bentcapPropsTMP(rightoverhang)
    bind $w.input.entryro <Return> "DefineBentcap_OK $w"
    grid $w.input.entryro -row $row -column 1
    label $w.input.unitro -text "in"
    grid $w.input.unitro -row $row -column 2 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineBentcap_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineBentcap_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineBentcap_Cancel {w} {
    AssignTMP_bentcapProps

    destroy $w
}

proc DefineBentcap_OK {w} {
    
    set ok [CheckBentcap]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_bentcapProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckBentcap {} {
    global bentcapPropsTMP

    set bcap $bentcapPropsTMP(bcap)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return width
    }

    set dcap $bentcapPropsTMP(dcap)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return depth
    }

    set dcap $bentcapPropsTMP(leftoverhang)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "left overhang"
    }

    set dcap $bentcapPropsTMP(rightoverhang)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "right overhang"
    }
}

proc Assign_soilProps {} {
    global soilProps
    global soilPropsTMP
    
    # Make all soil layers the same as per user input
    global setAllLayers
    global whichLayer
    global layerItems
    if {$setAllLayers != 0} {
       for {set i 1} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
          foreach item "gamma phi nh eps50 c type depth space multiplier" {
             set soilPropsTMP($item,$i) $soilPropsTMP($item,$whichLayer) 
          }
       }
    }
    set setAllLayers 0

    foreach var [array names soilPropsTMP] {
       if {[info exists soilProps($var)] && $soilProps($var) != $soilPropsTMP($var)} {
          ModelIsDirty
       }
       set soilProps($var) $soilPropsTMP($var)
    }
}

proc AssignTMP_soilProps {} {
    global soilProps
    global soilPropsTMP
    
    foreach var [array names soilProps] {
	set soilPropsTMP($var) $soilProps($var)
    }
}

set soilProps(Nlayers) 3
set soilProps(subgradeHeight) 60.0
set soilProps(waterTableDepth) 3.0
set soilProps(frozenSoilDepth) 8.0
set soilProps(gamma,default) 125.0
set soilProps(phi,default) 28.0
set soilProps(nh,default) 50.0
set soilProps(eps50,default) 0.02
set soilProps(c,default) 2.5
set soilProps(space,default) 1.0
set soilProps(multiplier,default) 1.0
for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
    set soilProps(depth,$i) [expr $soilProps(subgradeHeight)/$soilProps(Nlayers)]
    set soilProps(gamma,$i) $soilProps(gamma,default)
    set soilProps(phi,$i) $soilProps(phi,default)
    set soilProps(nh,$i) $soilProps(nh,default)
    set soilProps(type,$i) "Sand"
    set soilProps(eps50,$i) $soilProps(eps50,default)
    set soilProps(c,$i) $soilProps(c,default)
    set soilProps(space,$i) $soilProps(space,default)
    set soilProps(multiplier,$i) $soilProps(multiplier,default)
}
AssignTMP_soilProps


proc ChangeSoilLayers {} {
    global soilProps
    global soilPropsTMP

    if {$soilPropsTMP(Nlayers) <= 0} {
	set soilPropsTMP(Nlayers) 1
    }

    # 1. If adding new layers
    if {$soilPropsTMP(Nlayers) > $soilProps(Nlayers)} {
	# Do nothing if attempting to increase layers but decrease depth
	if {$soilPropsTMP(subgradeHeight) < $soilProps(subgradeHeight)} {
	    set soilPropsTMP(Nlayers) $soilProps(Nlayers)
	    set soilPropsTMP(subgradeHeight) $soilProps(subgradeHeight)
	    return
	}
	for {set i [expr $soilProps(Nlayers)+1]} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	    # Assign default values
	    set soilPropsTMP(gamma,$i) $soilProps(gamma,default)
	    set soilPropsTMP(phi,$i) $soilProps(phi,default)
	    set soilPropsTMP(nh,$i) $soilProps(nh,default)
	    set soilPropsTMP(type,$i) "Sand"
	    set soilPropsTMP(eps50,$i) $soilProps(eps50,default)
	    set soilPropsTMP(c,$i) $soilProps(c,default)
	    set soilPropsTMP(space,$i) $soilProps(space,default)
	    set soilPropsTMP(multiplier,$i) $soilProps(multiplier,default)
	    
	    # User is not also attempting to increase subgrade depth
	    if {$soilPropsTMP(subgradeHeight) == $soilProps(subgradeHeight)} {
		set soilPropsTMP(depth,$i) 10.0
		set soilPropsTMP(subgradeHeight) [expr $soilProps(subgradeHeight)+10.0]
	    } else {
		# Distribute new subgrade depth to new layers
		set soilPropsTMP(depth,$i) [expr ($soilPropsTMP(subgradeHeight)-$soilProps(subgradeHeight))/($soilPropsTMP(Nlayers)-$soilProps(Nlayers))]
	    }
	}
    }

    # 2. If removing layers
    if {$soilPropsTMP(Nlayers) < $soilProps(Nlayers)} {
	set sgh 0.0
	for {set i 1} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	    set sgh [expr $sgh + $soilPropsTMP(depth,$i)]
	}
	set soilPropsTMP(subgradeHeight) $sgh
    }

    # 3. If increasing subgrade depth, but not Nlayers
    if {$soilPropsTMP(subgradeHeight) > $soilProps(subgradeHeight) && $soilPropsTMP(Nlayers) == $soilProps(Nlayers)} {
	set increasedDepth [expr $soilPropsTMP(subgradeHeight)-$soilProps(subgradeHeight)]
	set N $soilPropsTMP(Nlayers)
	set soilPropsTMP(depth,$N) [expr $soilPropsTMP(depth,$N)+$increasedDepth]
    }

    # 4. If reducing subgrade depth, but not Nlayers
    if {$soilPropsTMP(subgradeHeight) < $soilProps(subgradeHeight) && $soilPropsTMP(Nlayers) == $soilProps(Nlayers)} {
	set y 0.0
	set sum 0.0
	for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	    set y [expr $y + $soilProps(depth,$i)]
	    if {$y >= $soilPropsTMP(subgradeHeight)} {
		break
	    }
	    set sum [expr $sum + $soilProps(depth,$i)]
	}
	set decreasedDepth [expr $soilPropsTMP(subgradeHeight)-$sum]

	set soilPropsTMP(depth,$i) $decreasedDepth
	set soilPropsTMP(Nlayers) $i ;# remove layers if necessary
    }

}

proc ChangeSoilLayerMenu {m} {
    global soilProps
    global soilPropsTMP

    for {set i [expr $soilProps(Nlayers)+1]} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	$m add command -label "Layer $i" -command "set whichLayer $i; DefineSoil .soil"
    }
    for {set i $soilProps(Nlayers)} {$i > $soilPropsTMP(Nlayers)} {incr i -1} {
	$m delete [expr $i+1] ;# For water table in first entry
    }
}

# tag -- of spring uniaxialMaterial
# depth -- to location of spring
# xTrib -- tributary length of pile
# pShadow -- p-multiplier for *shadowing*
proc OpenSeesSoilSpring {tag depth {xTrib 1.0} {pShadow 1.0}} {
    global units
    set lb $units(lb)
    set ft $units(ft)
    set ksi $units(ksi)
    set psi $units(psi)
    set pcf $units(pcf)
    set pci $units(pci)
    set in $units(in)
    set deg $units(deg)

    global soilPropsTMP
    set Nlayers $soilPropsTMP(Nlayers)
    set subgradeHeight $soilPropsTMP(subgradeHeight)
    set waterTableDepth $soilPropsTMP(waterTableDepth)
    
    set depthLimit 0.0
    for {set theLayer 1} {$theLayer <= $Nlayers} {incr theLayer} {
	set depthLimit [expr $depthLimit + $soilPropsTMP(depth,$theLayer)]
	if {$depth <= $depthLimit} {
	    break
	}
    }
    
    global sectionPropsTMP
    set b [expr $sectionPropsTMP(Dj,2)*$in]
    set gamma [expr $soilPropsTMP(gamma,$theLayer)*$pcf]
    set x $depth

    global analysisPropsTMP
    set subtractWater $analysisPropsTMP(subtractWater)
    set allElastic $analysisPropsTMP(allElastic)

    set gammaWater [expr {62.4*$pcf}]
    if {$subtractWater && $x > $waterTableDepth} {
       set gamma [expr $gamma-$gammaWater]
    }

    # this is a p-multiplier for the layer
    set pmult $soilPropsTMP(multiplier,$theLayer)

    if {$soilPropsTMP(type,$theLayer) == "N.C. Clay"} {
	set c [expr $soilPropsTMP(c,$theLayer)*$psi]
	set J 0.5
	set eps50 $soilPropsTMP(eps50,$theLayer)
	
	set pu [expr {(3 + $gamma/$c*$x + $J/$b*$x)*$c*$b}]
	if {$pu > [expr {9*$c*$b}]} {
	    set pu [expr {9*$c*$b}]
	}
	set y50 [expr {2.5*$eps50*$b}]
	
	hystereticBackbone ReeseSoftClay $tag [expr {$pu*$xTrib}] $y50 3.0
	
    } elseif {$soilPropsTMP(type,$theLayer) == "O.C. Clay"} {
	set c [expr $soilPropsTMP(c,$theLayer)*$psi]
	set eps50 $soilPropsTMP(eps50,$theLayer)

      # Below water table
	if {$x > $waterTableDepth} {
	    set nh [expr $soilPropsTMP(nh,$theLayer)*$pci]

	    set pc [expr {2*$c*$b + $gamma*$b*$x + 2.83*$c*$x}]
	    if {$pc > [expr {11*$c*$b}]} {
		set pc [expr {11*$c*$b}]
	    }

	    set kx [expr {$nh*$x}]
	    
	    set xOverb [expr {$x/$b}]
	    
	    if {$xOverb >= 4.0} {
		set As 0.6
	    } elseif {$xOverb >= 2.0} {
		set As [expr {0.55 + 0.025*($xOverb-2.0)}]
	    } else {
		set As [expr {0.2 + 0.175*$xOverb}]
	    }
	    
	    set y50 [expr {$eps50*$b}]

	    hystereticBackbone ReeseStiffClayBelowWS $tag $kx $y50 $As [expr {$pc*$xTrib}]

	} else {
	    set J 0.5
	 
	    set pu [expr {(3 + $gamma/$c*$x + $J/$b*$x)*$c*$b}]
	    if {$pu > [expr {9*$c*$b}]} {
		set pu [expr {9*$c*$b}]
	    }
	    set y50 [expr {2.5*$eps50*$b}]

	    hystereticBackbone ReeseSoftClay $tag [expr {$pu*$xTrib}] $y50 4.0
	}

    } elseif {$soilPropsTMP(type,$theLayer) == "Sand"} {
	set phi [expr {$soilPropsTMP(phi,$theLayer)*$deg}]
	
	set nh [expr {$soilPropsTMP(nh,$theLayer)*$pci}]
	# Modify if below water table
	if {$x > $waterTableDepth} {
	    set nh [expr {0.5184*$nh + 9.5797*$pci}]
	}

	set alpha [expr {0.5*$phi}]
	set beta [expr {45.0*$deg + $alpha}]
	set Ko 0.4
	set Ka [expr pow(tan(45.0*$deg-$phi/2),2)]

	set pst [expr {$Ko*$x*tan($phi)*sin($beta)/(tan($beta-$phi)*cos($alpha)) + tan($beta)/tan($beta-$phi)*($b+$x*tan($beta)*tan($alpha)) + $Ko*$x*tan($beta)*(tan($phi)*sin($beta)-tan($alpha)) - $Ka*$b}]
	set pst [expr {$gamma*$x*$pst}]

	set psd [expr {$Ka*$b*$gamma*$depth*(pow(tan($beta),8)-1) + $Ko*$b*$gamma*$depth*tan($phi)*pow(tan($beta),4)}]

	set ps $pst
	if {$psd < $pst} {set ps $psd}

	set kx [expr {$nh*$x}]

	set xOverb [expr {$x/$b}]

	if {$xOverb >= 4.0} {
	    set As 0.88
	} elseif {$xOverb >= 3.0} {
	    set As 1.0
	} elseif {$xOverb >= 2.0} {
	    set As [expr {1.5 - 0.5*($xOverb-2.0)}]
	} else {
	    set As [expr {2.8 - 0.65*$xOverb}]
	}
	set yu [expr {3.0*$b/80.0}]
	set pu [expr {$As*$ps}]

	if {$xOverb >= 4.0} {
	    set Bs 0.5
	} elseif {$xOverb >= 1.5} {
	    set Bs [expr {1.25 - 0.3*($xOverb-1.5)}]
	} else {
	    set Bs [expr {2.25 - 0.6667*$xOverb}]
	}
	set ym [expr {$b/60.0}]
	set pm [expr {$Bs*$ps}]

	hystereticBackbone ReeseSand $tag $kx $ym [expr {$pm*$xTrib}] $yu [expr {$pu*$xTrib}]
    }

    uniaxialMaterial Backbone -$tag $tag
    uniaxialMaterial Multiplier $tag -$tag [expr {$pShadow*$pmult}]

    return [expr {0.2*$b}]
}

set waterTableWindowOpen 0
proc DefineWaterTable {w} {

    global waterTableWindowOpen
    if {$waterTableWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Water Table"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0)  "The input value is the depth from the ground surface to the water table."
    
    createHelp $w $w.mbar water text


    set waterTableWindowOpen 1
    bind $w <Destroy> {set waterTableWindowOpen 0}
    
    global whichColumn

    AssignTMP_soilProps

    frame $w.input

    label $w.input.col -text "Depth to Water Table" 
    grid $w.input.col -row 0 -column 0 -columnspan 2
    entry $w.input.load -width 5 -textvariable soilPropsTMP(waterTableDepth)
    bind $w.input.load <Return> "DefineWaterTable_OK $w"
    grid $w.input.load -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineWaterTable_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineWaterTable_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineWaterTable_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineWaterTable_OK {w} {
    
    set ok [CheckWaterTable]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckWaterTable {} {
    global soilPropsTMP

    set wtd $soilPropsTMP(waterTableDepth)
    if {![isValidDouble $wtd] || $wtd < 0.0} {
	return "water table depth"
    }

}


set frozenSoilWindowOpen 0
proc DefineFrozenSoil {w} {

    global frozenSoilWindowOpen
    if {$frozenSoilWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Frozen Soil"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0)  "The input value is the depth from the ground surface to frozen soil."
    
    createHelp $w $w.mbar frozensoil text


    set frozenSoildWindowOpen 1
    bind $w <Destroy> {set frozenSoilWindowOpen 0}
    
    global whichColumn

    AssignTMP_soilProps

    frame $w.input

    label $w.input.col -text "Depth to Frozen Soil" 
    grid $w.input.col -row 0 -column 0 -columnspan 2
    entry $w.input.load -width 5 -textvariable soilPropsTMP(frozenSoilDepth)
    bind $w.input.load <Return> "DefineFrozenSoil_OK $w"
    grid $w.input.load -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineFrozenSoil_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineFrozenSoil_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineFrozenSoil_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineFrozenSoil_OK {w} {
    
    set ok [CheckFrozenSoil]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckFrozenSoil {} {
    global soilPropsTMP

    set fsd $soilPropsTMP(frozenSoilDepth)
    if {![isValidDouble $fsd] || $fsd < 0.0} {
	return "frozen soil depth"
    }

}


# Create a canvas to view the soil properties
set setAllLayers 0
set soilWindowOpen 0
proc DefineSoil {w} {
    global soilWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$soilWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Soil Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "Select the soil type for the indicated layer in order to edit its properties."

    createHelp $w $w.mbar soilType text


    set soilWindowOpen 1
    bind $w <Destroy> {set soilWindowOpen 0}

    AssignTMP_soilProps
    
    global soilPropsTMP
    global whichLayer

    frame $w.input

    set row 0

    label $w.input.labellayer -text "Soil Layer $whichLayer"
    grid $w.input.labellayer -row $row -column 0 -columnspan 3

    incr row

    label $w.input.labeldepth -text "Depth"
    grid $w.input.labeldepth -row $row -column 0
    entry $w.input.entrydepth -textvariable soilPropsTMP(depth,$whichLayer) -width 5
    bind $w.input.entrydepth <Return> "DefineSoil_OK $w"
    grid $w.input.entrydepth -row $row -column 1 -sticky e
    label $w.input.unitsdepth -text "ft"
    grid $w.input.unitsdepth -row $row -column 2 -sticky w

    incr row

    label $w.input.labelmult -text "Multiplier"
    grid $w.input.labelmult -row $row -column 0
    entry $w.input.entrymult -textvariable soilPropsTMP(multiplier,$whichLayer) -width 5
    bind $w.input.entrymult <Return> "DefineSoil_OK $w"
    grid $w.input.entrymult -row $row -column 1 -sticky e
    #label $w.input.unitsmult -text ""
    #grid $w.input.unitsmult -row $row -column 2 -sticky w

    incr row

    label $w.input.labelspacing -text "p-y Spacing"
    grid $w.input.labelspacing -row $row -column 0
    entry $w.input.entryspacing -textvariable soilPropsTMP(space,$whichLayer) -width 5 -state disabled
    bind $w.input.entryspacing <Return> "DefineSoil_OK $w"
    grid $w.input.entryspacing -row $row -column 1 -sticky e
    label $w.input.unitsspacing -text "ft"
    grid $w.input.unitsspacing -row $row -column 2 -sticky w

    set m [tk_optionMenu $w.input.soilmenu soilPropsTMP(type,$whichLayer) junk]
    $m delete 0
    #$m entryconfigure 0 -command {puts HELLO} ;# This works too

    $m insert 0 radiobutton -label "Sand" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input Sand"
    $m insert 1 radiobutton -label "N.C. Clay" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input SoftClay"
    $m insert 2 radiobutton -label "O.C. Clay" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input StiffClay"

    incr row

    label $w.input.type -text "Type"
    grid $w.input.type -row $row -column 0
    grid $w.input.soilmenu -row $row -column 1 -columnspan 2

    incr row

    foreach {lbl ent unt} {"gamma" gamma "pcf"  "phi" phi "deg"  "c" c "psi"  "eps50" eps50 "in/in"  "nh" nh "pci"} {
	label $w.input.label$row -text $lbl -anchor e
	grid $w.input.label$row -row $row -column 0
	entry $w.input.entry$row -width 5 -textvariable soilPropsTMP($ent,$whichLayer)
	bind $w.input.entry$row <Return> "DefineSoil_OK $w"
	grid $w.input.entry$row -row $row -column 1
	label $w.input.units$row -text $unt
	grid $w.input.units$row -row $row -column 2 -sticky w
	incr row
    }

    EnableSoilButtons $w.input $soilPropsTMP(type,$whichLayer)

    global setAllLayers
    set setAllLayers 0

    checkbutton $w.input.all -text "Apply to all soil layers" -variable setAllLayers
    grid $w.input.all -row $row -column 0 -columnspan 3 -sticky ew

    incr row

    button $w.input.py -text "P-Y Curves" -command "SoilPY .soilpy $w"
    grid $w.input.py -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSoil_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSoil_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineSoil_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineSoil_OK {w} {
    
    global soilPropsTMP
    set Nlayers $soilPropsTMP(Nlayers)

    for {set i 1} {$i <= $Nlayers} {incr i} {
       set ok [CheckSoil $i]
       if {[string length $ok] > 0} {
          set ok "Layer $i -- $ok"
          break
       }
    }

    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input: $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSoil {whichLayer} {

    global soilPropsTMP

    set s $soilPropsTMP(space,$whichLayer)
    if {![isValidDouble $s] || $s <= 0.0} {
	return "p-y spring spacing"
    }

    set depth $soilPropsTMP(depth,$whichLayer)
    if {![isValidDouble $depth] || $depth <= 0.0} {
	return "layer depth"
    }

    set multiplier $soilPropsTMP(multiplier,$whichLayer)
    if {![isValidDouble $multiplier] || $multiplier <= 0.0} {
	return "p-multiplier"
    }

    set nh $soilPropsTMP(nh,$whichLayer)
    if {![isValidDouble $nh] || $nh <= 0.0} {
	return nh
    }

    set c $soilPropsTMP(c,$whichLayer)
    if {![isValidDouble $c] || $c <= 0.0} {
	return c
    }

    set eps50 $soilPropsTMP(eps50,$whichLayer)
    if {![isValidDouble $eps50] || $eps50 <= 0.0} {
	return eps50
    }

    set phi $soilPropsTMP(phi,$whichLayer)
    if {![isValidDouble $phi] || $phi <= 0.0 || $phi >= 90.0} {
	return phi
    }

    set gamma $soilPropsTMP(gamma,$whichLayer)
    if {![isValidDouble $gamma] || $gamma <= 0.0} {
	return gamma
    }

    set Nlayers $soilPropsTMP(Nlayers)
    set endDepth 0
    for {set i 1} {$i <= $whichLayer} {incr i} {
       set depth $soilPropsTMP(depth,$i)
       set endDepth [expr {$endDepth + $depth}]
    }
    
    global analysisProps
    set buoyantWeight $analysisProps(subtractWater)

    set waterTableDepth $soilPropsTMP(waterTableDepth)

    global units
    set pcf $units(pcf)
    set gamma [expr {$gamma*$pcf}]
    set gammaWater [expr {62.4*$pcf}]

    if {$buoyantWeight && $waterTableDepth < $endDepth && $gamma < $gammaWater} {
       return "gammaSoil < gammaWater"
    }




}

proc EnableSoilButtons {w type} {

   for {set row 5} {$row <= 9} {incr row} {
      $w.entry$row config -state disabled
   }

   set whichRows ""
   if {$type == "Sand"} {
      set whichRows "5 6 9"
   }
   if {$type == "SoftClay" || $type == "Soft Clay" || $type == "N.C. Clay"} {
      set whichRows "5 7 8"
   }
   if {$type == "StiffClay" || $type == "Stiff Clay" || $type == "O.C. Clay"} {
      set whichRows "5 7 8 9"
   }

   foreach row $whichRows {
      $w.entry$row config -state normal
   }

}

set soilPYWindowOpen 0
proc SoilPY {w p} {

    global whichLayer

    set ok [CheckSoil $whichLayer]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global soilPYWindowOpen
    if {$soilPYWindowOpen == 1} {raise $w; return}

    global units
    set ksi $units(ksi)
    set ft $units(ft)

    global soilPropsTMP

    createTopLevel $w "Soil P-Y Analysis"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"

    $m add cascade -label "Export" -menu $m.soils
    set m2 [menu $m.soils]

    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set soilPYWindowOpen 1
    bind $w <Destroy> {set soilPYWindowOpen 0}

    set startDepth 0.0
    for {set i 1} {$i < $whichLayer} {incr i} {
	set startDepth [expr $startDepth + $soilPropsTMP(depth,$i)]
    }
    set layerDepth $soilPropsTMP(depth,$whichLayer)
    set endDepth [expr $startDepth + $layerDepth]

    set spacing $soilPropsTMP(space,$whichLayer)
    set numSprings 0

    set NpyLayers [expr int($layerDepth/$spacing)]
    set r [expr 0.5*($layerDepth-$spacing*$NpyLayers)]

    # Do first spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$startDepth + 0.5*$r}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set coords($numSprings,depth) $depth
       incr numSprings
    }

    # Do springs in main p-y layes
    for {set i 1} {$i <= $NpyLayers} {incr i} {
       set depth [expr {$startDepth + $r + ($i-0.5)*$spacing}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $spacing]
       set coords($numSprings,depth) $depth
       incr numSprings
    }

    # Do last spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$endDepth - 0.5*$r}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set coords($numSprings,depth) $depth
       incr numSprings
    }


    set emax 0
    set smax 0
    for {set i 0} {$i < $numSprings} {incr i} {
       foreach {e s} $coords($i) {
          if {$e > $emax} {set emax $e}
          if {$s > $smax} {set smax $s}
       }
    }

    for {set k 0} {$k < $numSprings} {incr k} {
	set color blue
	set depth $coords($k,depth)
	if {$depth < $soilPropsTMP(waterTableDepth)} {
	    set color black
	}
	$graph data d$k -colour $color -points 0 -lines 1 -coords $coords($k)
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    for {set k 0} {$k < $numSprings} {incr k} {
	set depth $coords($k,depth)

	$m2 add command -label "Spring at x = $depth ft" -command "ExportData [list $coords($k)] $w"

	set x [lindex $coords($k) end-1]
	set y [lindex $coords($k) end]
	$c create text [$graph x2canvas $x] [$graph y2canvas $y] -text "x = $depth ft" -anchor w -tag dd$k
	$c bind dd$k <Double-Button-1> "ExportData [list $coords($k)] $w"
    }
    
    set graphInfo(title) "Soil Layer $whichLayer ($soilPropsTMP(type,$whichLayer)), Multiplier = $soilPropsTMP(multiplier,$whichLayer)"
    set graphInfo(xlabel) "y (ft)"
    set graphInfo(ylabel) "p (kip/ft)"
    labelEMUGraph $c $graph graphInfo $width $height


    frame $w.output

    text $w.output.text -width 15
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    $w.output.text insert end "Use the File->Export menu or double-click on a depth, e.g., x = 2.5 ft to export data"

    $w.output.text configure -state disabled
}

proc OpenSeesSoilAnalysis {depth {xTrib 1.0}} {

	wipe

	model basic -ndm 1 -ndf 1
	
	node 1 0.0
	node 2 1.0
	
	fix 2 1
	
	set maxDisp [OpenSeesSoilSpring 1 $depth $xTrib]

	element truss 1 1 2 1.0 1
	
	pattern Plain 1 Linear {
	    load 1 1.0
	}
	
	integrator DisplacementControl 1 1 [expr 0.01*$maxDisp]
	constraints Plain
	test NormUnbalance 1.0e-8 10 0
	algorithm Newton
	numberer Plain
	system UmfPack
       
	analysis Static
	
	set coords "0 0"
	
	set ok 0
	set j 0
	
	while {$ok >= 0 && [nodeDisp 1 1] < $maxDisp} {
	    
	    set ok [analyze 1]
	    
	    set strain [nodeDisp 1 1]
	    set stress [getTime]
	    lappend coords $strain $stress
	    
	    incr j
	}

      return $coords

}

proc Assign_eqProps {} {
    global eqProps
    global eqPropsTMP    

    foreach var [array names eqPropsTMP] {
       if {[info exists eqProps($var)] && $eqProps($var) != $eqPropsTMP($var)} {
          ModelIsDirty
       }
       set eqProps($var) $eqPropsTMP($var)
    }

    foreach var [array names eqPropsTMP] {
	set eqProps($var) $eqPropsTMP($var)
    }
}

proc AssignTMP_eqProps {} {
    global eqProps
    global eqPropsTMP

    foreach var [array names eqProps] {
	set eqPropsTMP($var) $eqProps($var)
    }
}

set eqProps(PGA) 1.0
set eqProps(dtGM) 0.01
set eqProps(filename) "None Selected"

AssignTMP_eqProps


set eqMotionWindowOpen 0
proc DefineEQMotion {w} {

    global eqMotionWindowOpen
    if {$eqMotionWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Earthquake Motion"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    #set text(0) "The input value is the gravity load applied to the indicated column and held constant during the pushover analysis."
    #set text(1) "Positive values indicate compression while negative values indicate tension."

    #createHelp $w $w.mbar gravityLoad text


    set eqMotionWindowOpen 1
    bind $w <Destroy> {set eqMotionWindowOpen 0}    

    global eqProps
    global eqPropsTMP

    foreach var [array names eqProps] {
	set eqPropsTMP($var) $eqProps($var)
    }

    frame $w.input

    label $w.input.labels -text "PGA"
    grid $w.input.labels -row 0 -column 0 -sticky e
    entry $w.input.entrys -width 5 -textvariable eqPropsTMP(PGA)
    bind $w.input.entrys <Return> "DefineEQMotion_OK $w"
    grid $w.input.entrys -row 0 -column 1
    label $w.input.units -text "g"
    grid $w.input.units -row 0 -column 2 -sticky w

    label $w.input.labelb -text "Record Time Step"
    grid $w.input.labelb -row 1 -column 0 -sticky e
    entry $w.input.entryb -width 5 -textvariable eqPropsTMP(dtGM)
    bind $w.input.entryb <Return> "DefineEQMotion_OK $w"
    grid $w.input.entryb -row 1 -column 1
    label $w.input.unitb -text "sec"
    grid $w.input.unitb -row 1 -column 2 -sticky w

    button $w.input.buttong -text "Ground Motion" -command "LoadEQMotion $w"
    grid $w.input.buttong -row 2 -column 0 -columnspan 3 -sticky ew

    label $w.input.labelgm -width 20 -textvariable eqPropsTMP(filename) -anchor e
    grid $w.input.labelgm -row 3 -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineEQMotion_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineEQMotion_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc LoadEQMotion {w} {
    global wd
    global eqPropsTMP
    global EQtypelist

    set openFile [tk_getOpenFile -defaultextension .txt -filetypes $EQtypelist -initialdir $wd -parent $w]
    if {[llength $openFile] <= 0} {
	return
    }

    set eqPropsTMP(filename) $openFile
}

proc DefineEQMotion_Cancel {w} {
    AssignTMP_eqProps

    destroy $w
}

proc DefineEQMotion_OK {w} {
    
    set ok [CheckEQMotion]

    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_eqProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckEQMotion {} {
    global eqPropsTMP

    set x $eqPropsTMP(PGA)
    if {![isValidDouble $x] || $x <= 0.0} {
	return "ground motion PGA"
    }
    set x $eqPropsTMP(dtGM)
    if {![isValidDouble $x] || $x <= 0.0} {
	return "ground motion record time digitization"
    }
}

proc CenterText {p text {xOffset 0}} {
   global pageInfo
   set xCenter $pageInfo(xCenter)
   if {$xOffset != 0} {
      set xCenter $xOffset
   }
   $p text $text -x [expr ($xCenter - [$p getStringWidth $text]/2.0)]
}
proc LeftText {p text {xOffset 0}} {
   global pageInfo
   set xLeft $pageInfo(xLeft)
   $p text $text -x [expr $xLeft + $xOffset]
}
proc RightText {p text {xOffset 0}} {
   global pageInfo
   set xRight $pageInfo(xRight)
   if {$xOffset != 0} {
      set xRight $xOffset
   }
   $p text $text -x [expr ($xRight - [$p getStringWidth $text])]
}
proc WriteHeader {p} {
   global pageInfo
   $p setFont 10 "Times-Roman"
   $p setTextPosition 0 [expr -0.5*$pageInfo(margin)]
   RightText $p "[BridgeName]"; $p newLine
   RightText $p "[EngineerName]"
}
proc WritePageNumber {p num} {
   global pageInfo
   $p setFont 10 "Times-Roman"
   $p setTextPosition 0 [expr $pageInfo(ySize)-1.5*$pageInfo(margin)]
   CenterText $p $num 
}
proc ReportDataLine {p label value {units ""}} {
   global pageInfo
   set xCenter $pageInfo(xCenter)
   LeftText $p $label; RightText $p $value $xCenter; LeftText $p " $units" $xCenter
}

proc CreateReport {w} {

    global PDFtypelist
    global wd
    global pageInfo

    if {[IsModelDirty] || [IsSectionDirty]} {
       set answer [tk_messageBox -message "Bridge model has changed since last save.  Save now?"             -icon question -type yesnocancel -default yes -title "Save Bridge file"]

       switch $answer {
          yes {
             SaveFile
             set tmp [tk_messageBox -message "Bridge file saved, click OK to create report"                   -icon info -type ok -default ok -title "Create Report"]
          }
          no {
             set tmp [tk_messageBox -message "Bridge file not saved, click OK to create report"                   -icon info -type ok -default ok -title "Create Report"]
          }
          cancel return
       }
    }

    set openFile [tk_getSaveFile -filetypes $PDFtypelist -initialdir $wd -initialfile [BridgeName]          -defaultextension .pdf -parent . -title "Create Report"]
    
    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    set iReport 0
    set NreportSteps 18

    global stopReport
    set stopReport 0

    createTopLevel $w "Summary Report"

    set reportWindowOpen 1
    bind $w <Destroy> {set reportWindowOpen 0; set stopReport 1}

    frame $w.report
    label $w.report.label
    grid $w.report.label -row 0 -column 0 -sticky ew
    label $w.report.update
    grid $w.report.update -row 1 -column 0 -sticky ew

    set stopReport 0
    button $w.report.cancel -text "Stop" -command "set stopReport 1"
    grid $w.report.cancel -row 2 -column 0 -sticky ew

    label $w.report.warning -text "Please disregard changes to main screen \n during report generation"
    grid $w.report.warning -row 3 -column 0 -sticky ew

    pack $w.report -side top



# A canvas/graph for drawing plots in report
set gWidth 600
set gHeight 600
set graph foo
set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set size [pdf4tcl::getPaperSize a4]
set xSize [lindex $size 0]
set ySize [lindex $size 1]
set pageInfo(xSize) $xSize
set pageInfo(ySize) $ySize

set margin 72.0
set pageInfo(margin) $margin
set pageInfo(xLeft) 0
set xCenter [expr $xSize/2.0 - $margin]
set pageInfo(xCenter) $xCenter
set pageInfo(xRight) [expr $xSize - 2.0*$margin]

pdf4tcl::new p1 -paper a4 -margin $margin

set pageNumber 0

#----- Title Page -----

$w.report.label config -text "Creating Title Page"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage 
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 [BridgeName]; p1 newLine
p1 newLine
CenterText p1 "Bridge Analysis Summary"; p1 newLine

for {set i 1} {$i <= 12} {incr i} {
    p1 newLine
}
p1 setFont 14
CenterText p1 [EngineerName]; p1 newLine
CenterText p1 [DateTime]; p1 newLine


for {set i 1} {$i <= 6} {incr i} {
   p1 newLine
}
p1 setFont 12
CenterText p1 "[SoftwareTitle], v[VersionNumber]"; p1 newLine
for {set i 1} {$i <= 6} {incr i} {
   p1 newLine
}

p1 setFont 14
CenterText p1 "Contents"; p1 newLine
p1 setFont 12
LeftText p1 "Bent Overview"; RightText p1 2; p1 newLine
LeftText p1 "Transverse Pushover Analysis"; RightText p1 3; p1 newLine
LeftText p1 "Longitudinal Pushover Analysis"; RightText p1 7; p1 newLine
global sectionProps
if {$sectionProps(pileSameAsColumn) == 1} {
    LeftText p1 "CISS/Pile Section Details"; RightText p1 11; p1 newLine
    set page 11
} else {
    LeftText p1 "CISS Section Details"; RightText p1 11; p1 newLine
    LeftText p1 "Pile Section Details"; RightText p1 12; p1 newLine
    set page 12
}
LeftText p1 "Gap Section Moment-Curvature"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Section Axial-Moment Interaction"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Longitudinal Reinforcing Steel"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Steel Shell"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Concrete"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Soil Layers"; RightText p1 [incr page]; p1 newLine

incr pageNumber

#----- Picture of Bent -----

$w.report.label config -text "Creating Bent Image"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Bent Overview"; p1 newLine
p1 newLine
p1 setFont 12

global columnProps
ReportDataLine p1 "Column Height"            [format %.1f $columnProps(L)] "ft above grade"; p1 newLine
global bentcapProps
ReportDataLine p1 "Cap Beam depth"           [format %.1f $bentcapProps(dcap)] "in"; p1 newLine
ReportDataLine p1 "Cap Beam width"           [format %.1f $bentcapProps(bcap)] "in"; p1 newLine
ReportDataLine p1 "Cap Overhang left"        [format %.1f $bentcapProps(leftoverhang)] "in (clear from column face)"; p1 newLine
ReportDataLine p1 "Cap Overhang right"       [format %.1f $bentcapProps(rightoverhang)] "in (clear from column face)"; p1 newLine
global hingeProps
ReportDataLine p1 "Shell Gap, Gf"            [format %.1f $hingeProps(Gf)] in; p1 newLine
ReportDataLine p1 "Plastic Hinge Length, lp" [format %.2f $hingeProps(lp)] ft; p1 newLine
global soilProps
ReportDataLine p1 "Water Table"              [format %.1f $soilProps(waterTableDepth)] "ft below grade"; p1 newLine
#ReportDataLine p1 "Frozen Soil"              [format %.1f $soilProps(frozenSoilDepth)] "ft below grade"; p1 newLine
ReportDataLine p1 "Total Soil Depth"         [format %.1f $soilProps(subgradeHeight)]  "ft below grade"; p1 newLine

set c .tmpc
destroy $c
canvas $c -height 400 -width 400
set width [winfo reqwidth $c]
set height [winfo reqheight $c]

DefineModel $c off

p1 canvas $c -y [expr 2*18 + 7*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

clearCanvas $c

WritePageNumber p1 [incr pageNumber]

#----- Transverse Pushover Analysis -----

$w.report.label config -text "Transverse Pushover Analysis"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Transverse Pushover Analysis"; p1 newLine
p1 newLine
p1 setFont 12

set coords [OpenSeesPushoverAnalysis T]

global units
set ft $units(ft)
set in $units(in)

global analysisProps
global columnProps
set maxLateralDisp [expr $analysisProps(lateralDisp,T)*$ft]
set whichLimit ""

global sectionProps
set Dj [expr $sectionProps(Dj,1)*$in]

if {$Dj > $maxLateralDisp} {
   #set maxLateralDisp $Dj
   #set whichLimit "(Column Diameter)"
}

ReportDataLine p1 "Max Displacement $whichLimit" $maxLateralDisp ft
p1 newLine

set PDyn On
if {$analysisProps(pDelta) == 0} {
   set PDyn Off
}
ReportDataLine p1 "P-Delta Analysis" $PDyn
p1 newLine

set SSIyn On
if {$analysisProps(soilStructure) == 0} {
   set SSIyn Off
}
ReportDataLine p1 "Soil-Structure Interaction" $SSIyn
p1 newLine

set H2Oyn On
if {$analysisProps(subtractWater) == 0} {
   set H2Oyn Off
}
ReportDataLine p1 "Soil Buoyant Weight below GWT" $H2Oyn
p1 newLine

ReportDataLine p1 "Overstrength Factor" $analysisProps(overstrength)
p1 newLine
p1 newLine

global analysisStates

ReportDataLine p1 "R/F Steel Yield"  " "
if {$analysisStates(steelYield,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelYield,T,disp)] ft, [format %.1f $analysisStates(steelYield,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "R/F Steel Strain Limit ($analysisStates(steelLimit,val) in/in)"  " "
if {$analysisStates(steelLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelLimit,T,disp)] ft, [format %.1f $analysisStates(steelLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Steel Shell Strain Limit ($analysisStates(jacketLimit,val) in/in)"  " "
if {$analysisStates(jacketLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(jacketLimit,T,disp)] ft, [format %.1f $analysisStates(jacketLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Concrete Strain Limit ($analysisStates(concreteLimit,val) in/in)"  " "
if {$analysisStates(concreteLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(concreteLimit,T,disp)] ft, [format %.1f $analysisStates(concreteLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Peak Capacity" " "
if {$analysisStates(maxLoad,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(maxLoad,T,disp)] ft, [format %.1f $analysisStates(maxLoad,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

global analysisProps

ReportDataLine p1 "Post Peak Limit ($analysisProps(postPeak)\% Peak)"  " "
if {$analysisStates(postPeak,T,disp) > 0} {
   RightText p1 "@ [format %.1f $analysisStates(postPeak,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

PlotLimitStates $graph $gCanvas T

set graphInfo(title) ""
set graphInfo(xlabel) "Displacement (ft)"
set graphInfo(ylabel) "Load (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Final Displaced Shape -----

$w.report.label config -text "Drawing Final Displaced Shape"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Displaced Shape"; p1 newLine
p1 newLine
p1 setFont 12

global extremeAxialLoad

set TorL T

ReportDataLine p1 "Extreme Axial Loads" " "
global columnProps
global extremeAxialLoad
for {set i 1} {$i <= $columnProps(N)} {incr i} {
    puts $extremeAxialLoad($i,max,$TorL)
    puts $extremeAxialLoad($i,min,$TorL)
    set tmpMin [format %.1f $extremeAxialLoad($i,min,$TorL)]
    set tmpMax [format %.1f $extremeAxialLoad($i,max,$TorL)]
    set outputString "Column $i:"
    if {$tmpMin >= 0} {
       set outputString "$outputString $tmpMin kip (C)"
    } else {
       set outputString "$outputString [expr abs($tmpMin)] kip (T)"
    }
    if {$tmpMax >= 0} {
       set outputString "$outputString to $tmpMax kip (C)"
    } else {
       set outputString "$outputString to [expr abs($tmpMax)] kip (T)"
    }
    RightText p1 $outputString
    p1 newLine
}

p1 newLine

set N $columnProps(N)

global bentCapForces
# Make sure there is more than one column
if {$N > 1} {
    ReportDataLine p1 "Bent Cap Forces" " "
    set N [format %.1f $bentCapForces(trail,N)]
    set TorC T
    if {$N < 0} {set TorC C}
    set V [format %.1f $bentCapForces(trail,V)]
    set M [format %.1f $bentCapForces(trail,M)]
    RightText p1 "Inside trailing pile: N=[expr abs($N)] kip ($TorC), V=$V kip, M=$M kip-ft"
    p1 newLine
    set N [format %.1f $bentCapForces(lead,N)]
    set TorC T
    if {$N < 0} {set TorC C}
    set V [format %.1f $bentCapForces(lead,V)]
    set M [format %.1f $bentCapForces(lead,M)]
    RightText p1 "Inside leading pile: N=[expr abs($N)] kip ($TorC), V=$V kip, M=$M kip-ft"
    p1 newLine
    if {$analysisProps(lateralLoad) == 1} {
	RightText p1 "Lateral load DISTRIBUTED across bent cap"
    } else {
	RightText p1 "Lateral load CONCENTRATED at bent cap-column joints"	
    }
}


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawDisplacedShape $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Final Moment Diagram -----

$w.report.label config -text "Drawing Final Moment Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Moment Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawMomentDiagram $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]



#----- Final Shear Diagram -----

$w.report.label config -text "Drawing Final Shear Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Shear Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawShearDiagram $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Longitudinal Pushover Analysis -----

$w.report.label config -text "Longitudinal Pushover Analysis"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Longitudinal Pushover Analysis"; p1 newLine
p1 newLine
p1 setFont 12

set coords [OpenSeesPushoverAnalysis L]

global units
set ft $units(ft)
set in $units(in)

global analysisProps
global columnProps
set maxLateralDisp [expr $analysisProps(lateralDisp,L)*$ft]
set whichLimit ""

global sectionProps
set Dj [expr $sectionProps(Dj,1)*$in]

if {$Dj > $maxLateralDisp} {
   #set maxLateralDisp $Dj
   #set whichLimit "(Column Diameter)"
}

ReportDataLine p1 "Max Lateral Disp $whichLimit" $maxLateralDisp ft
p1 newLine

set PDyn On
if {$analysisProps(pDelta) == 0} {
   set PDyn Off
}
ReportDataLine p1 "P-Delta Analysis" $PDyn
p1 newLine

set SSIyn On
if {$analysisProps(soilStructure) == 0} {
   set SSIyn Off
}
ReportDataLine p1 "Soil-Structure Interaction" $SSIyn
p1 newLine

set H2Oyn On
if {$analysisProps(subtractWater) == 0} {
   set H2Oyn Off
}
ReportDataLine p1 "Soil Buoyant Weight below GWT" $H2Oyn
p1 newLine

ReportDataLine p1 "Overstrength Factor" $analysisProps(overstrength)
p1 newLine
p1 newLine

global analysisStates

ReportDataLine p1 "R/F Steel Yield"  " "
if {$analysisStates(steelYield,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelYield,L,disp)] ft, [format %.1f $analysisStates(steelYield,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "R/F Steel Strain Limit ($analysisStates(steelLimit,val) in/in)"  " "
if {$analysisStates(steelLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelLimit,L,disp)] ft, [format %.1f $analysisStates(steelLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Steel Shell Strain Limit ($analysisStates(jacketLimit,val) in/in)"  " "
if {$analysisStates(jacketLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(jacketLimit,L,disp)] ft, [format %.1f $analysisStates(jacketLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Concrete Strain Limit ($analysisStates(concreteLimit,val) in/in)"  " "
if {$analysisStates(concreteLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(concreteLimit,L,disp)] ft, [format %.1f $analysisStates(concreteLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Peak Capacity" " "
if {$analysisStates(maxLoad,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(maxLoad,L,disp)] ft, [format %.1f $analysisStates(maxLoad,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

global analysisProps

ReportDataLine p1 "Post Peak Limit ($analysisProps(postPeak)\% Peak)"  " "
if {$analysisStates(postPeak,L,disp) > 0} {
   RightText p1 "@ [format %.1f $analysisStates(postPeak,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

PlotLimitStates $graph $gCanvas L

set graphInfo(title) ""
set graphInfo(xlabel) "Displacement (ft)"
set graphInfo(ylabel) "Load (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Final Displaced Shape -----

$w.report.label config -text "Drawing Final Displaced Shape"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Displaced Shape"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawDisplacedShape $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Final Moment Diagram -----

$w.report.label config -text "Drawing Final Moment Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Moment Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawMomentDiagram $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]



#----- Final Shear Diagram -----

$w.report.label config -text "Drawing Final Shear Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Shear Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawShearDiagram $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Section Details -----

$w.report.label config -text "Drawing Section Details"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}



p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
if {$sectionProps(pileSameAsColumn) == 1} {
    CenterText p1 "CISS/Pile Section Details"; p1 newLine
} else {
    CenterText p1 "CISS Section Details"; p1 newLine
}
p1 newLine
p1 setFont 12

global sectionProps

global units
set in $units(in)

set ds $sectionProps(ds,1)
set barArea [BarArea $ds]
set Nbars $sectionProps(Nbars,1)
set steelArea [expr {$Nbars*$barArea}]

set pi [expr {2*asin(1.0)}]
set Dj [expr {$sectionProps(Dj,1)*$in}]
set tj [expr {$sectionProps(tj,1)*$in}]
set Dc [expr {$Dj-2*$tj}]

set cover [expr {$sectionProps(cover,1)*$in}]

set sectionArea [expr {0.25*$pi*$Dc*$Dc}]
set rhol [expr {$steelArea/$sectionArea}]

# Now go back to original units for report
set Dj $sectionProps(Dj,1)
set tj $sectionProps(tj,1)
set cover $sectionProps(cover,1)

ReportDataLine p1 "Outer Shell Diamter, Dj" [format %.2f $Dj]    in; p1 newLine
ReportDataLine p1 "Shell Thickness, tj"     [format %.2f $tj]    in; p1 newLine
p1 newLine

if {$sectionProps(noConcrete,1) == 0} {
    ReportDataLine p1 "Clear Cover"             [format %.2f $cover] in; p1 newLine
    ReportDataLine p1 "Number of Bars"          $Nbars                 ; p1 newLine
    ReportDataLine p1 "Bar Size"                $ds                    ; p1 newLine
    ReportDataLine p1 "Long. R/F Ratio"         [format %.5f $rhol]    ; p1 newLine
    p1 newLine
    
    ReportDataLine p1 "Spiral Size"  $sectionProps(dshoop,1)    ; p1 newLine
    ReportDataLine p1 "Hoop Spacing" [format %.2f $sectionProps(sphoop,1)]  in; p1 newLine
    ReportDataLine p1 "Yield Stress" [format %.2f $sectionProps(fyhoop,1)] ksi; p1 newLine
} else {
    ReportDataLine p1 "No concrete in column" ""
}

global whichSection
set whichSection 1
DrawSection $c off

p1 canvas $c -y [expr 2*18 + 10*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

clearCanvas $c

WritePageNumber p1 [incr pageNumber]



if {$sectionProps(pileSameAsColumn) == 0} {
    p1 startPage
    WriteHeader p1
    p1 setTextPosition 0 0
    
    p1 setFont 18 "Times-Roman"
    CenterText p1 "Pile Section Details"; p1 newLine
    p1 newLine
    p1 setFont 12
    
    global sectionProps
    
    global units
    set in $units(in)
    
    set ds $sectionProps(ds,2)
    set barArea [BarArea $ds]
    set Nbars $sectionProps(Nbars,2)
    set steelArea [expr {$Nbars*$barArea}]
    
    set pi [expr {2*asin(1.0)}]
    set Dj [expr {$sectionProps(Dj,2)*$in}]
    set tj [expr {$sectionProps(tj,2)*$in}]
    set Dc [expr {$Dj-2*$tj}]
    
    set cover [expr {$sectionProps(cover,2)*$in}]
    
    set sectionArea [expr {0.25*$pi*$Dc*$Dc}]
    set rhol [expr {$steelArea/$sectionArea}]
    
    # Now go back to original units for report
    set Dj $sectionProps(Dj,2)
    set tj $sectionProps(tj,2)
    set cover $sectionProps(cover,2)
    
    ReportDataLine p1 "Outer Shell Diamter, Dj" [format %.2f $Dj]    in; p1 newLine
    ReportDataLine p1 "Shell Thickness, tj"     [format %.2f $tj]    in; p1 newLine
    p1 newLine

    if {$sectionProps(noConcrete,2) == 0} {
	ReportDataLine p1 "Clear Cover"             [format %.2f $cover] in; p1 newLine
	ReportDataLine p1 "Number of Bars"          $Nbars                 ; p1 newLine
	ReportDataLine p1 "Bar Size"                $ds                    ; p1 newLine
	ReportDataLine p1 "Long. R/F Ratio"         [format %.5f $rhol]    ; p1 newLine
	p1 newLine
	
	ReportDataLine p1 "Spiral Size"  $sectionProps(dshoop,2)    ; p1 newLine
	ReportDataLine p1 "Hoop Spacing" [format %.2f $sectionProps(sphoop,2)]  in; p1 newLine
	ReportDataLine p1 "Yield Stress" [format %.2f $sectionProps(fyhoop,2)] ksi; p1 newLine
    } else {
	ReportDataLine p1 "No concrete in pile" ""
    }

    global whichSection
    set whichSection 2
    DrawSection $c off
    
    p1 canvas $c -y [expr 2*18 + 10*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
    
    clearCanvas $c
    
    WritePageNumber p1 [incr pageNumber]
}


#----- Moment-Curvature @ Maximum Axial Load -----

$w.report.label config -text "Calculating M-phi at Max. Axial Load"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

global bentCapForces
global extremeAxialLoad
set maxAxialLoadT $extremeAxialLoad(1,max,T)
set whichMax 1
set whenMax $extremeAxialLoad(1,max,when,T)
set minAxialLoadT $extremeAxialLoad(1,min,T)
set whichMin 1
set whenMin $extremeAxialLoad(1,min,when,T)
for {set i 2} {$i <= $columnProps(N)} {incr i} {
   set axialLoad $extremeAxialLoad($i,max,T)
   if {$axialLoad > $maxAxialLoadT} {
      set maxAxialLoadT $axialLoad
      set whichMax $i
   }
   set axialLoad $extremeAxialLoad($i,min,T)
   if {$axialLoad < $minAxialLoadT} {
      set minAxialLoadT $axialLoad
      set whichMin $i
   }
}

set coords1 [OpenSeesMKAnalysis [GapSection] $maxAxialLoadT]
set coords2 [OpenSeesMKAnalysis [GapSection] $minAxialLoadT]
set maxAxialLoadL $extremeAxialLoad(1,max,L) ;# same for all columns in Long. direction and will not change much wrt time
set coords3 [OpenSeesMKAnalysis [GapSection] $maxAxialLoadL]

p1 setFont 18 "Times-Roman"
CenterText p1 "Gap Section Moment-Curvature"; p1 newLine
p1 newLine
p1 setFont 12

if {$hingeProps(coreConfined) == 1} {
   ReportDataLine p1 "Core Confinement" "Steel Shell"; p1 newLine
} else {
   ReportDataLine p1 "Core Confinement" "Spiral Hoop"; p1 newLine
}
if {$hingeProps(coverConfined) == 1} {
   ReportDataLine p1 "Cover Confinement" "Same as Core"; p1 newLine
} else {
   ReportDataLine p1 "Cover Confinement" "None"; p1 newLine
}
p1 newLine
ReportDataLine p1 "Target Ductility" $sectionProps(MKductility); p1 newLine
p1 newLine

set minAxialLoadT [format %.1f $minAxialLoadT]
set maxAxialLoadT [format %.1f $maxAxialLoadT]
set maxAxialLoadL [format %.1f $maxAxialLoadL]

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

ReportDataLine p1 "Extreme Axial Loads" " "
if {$minAxialLoadT >= 0} {
   RightText p1 "Transverse Min: $minAxialLoadT kip (C) in Column $whichMin @ [format %.1f $extremeAxialLoad($whichMin,min,when,T)] kip"
} else {
   RightText p1 "Transverse Min: [expr abs($minAxialLoadT)] kip (T) in Column $whichMin @ [format %.1f $extremeAxialLoad($whichMin,min,when,T)] kip"
}
p1 newLine
if {[llength $coords2] <= 4} {
   RightText p1 "WARNING: Axial load ($minAxialLoadT kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d2 -colour red  -points 0 -lines 1 -coords $coords2
}

if {$maxAxialLoadT >= 0} {
   RightText p1 "Transverse Max: $maxAxialLoadT kip (C) in Column $whichMax @ [format %.1f $extremeAxialLoad($whichMax,max,when,T)] kip"
} else {
   RightText p1 "Transverse Max: [expr abs($maxAxialLoadT)] kip (T) in Column $whichMax @ [format %.1f $extremeAxialLoad($whichMax,max,when,T)] kip"
}
p1 newLine
if {[llength $coords1] <= 4} {
   RightText p1 "WARNING: Axial load ($maxAxialLoadT kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d1 -colour blue -points 0 -lines 1 -coords $coords1
}

if {$maxAxialLoadL >= 0} {
   RightText p1 "Longitudinal: $maxAxialLoadL kip (C)"
} else {
   RightText p1 "Longitudinal: [expr abs($maxAxialLoadL)] kip (T)"
}
p1 newLine
if {[llength $coords1] <= 4} {
   RightText p1 "WARNING: Axial load ($maxAxialLoadL kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d3 -colour black -points 0 -lines 1 -coords $coords3
}

set emax 0
set smax 0
foreach {e s} "$coords1 $coords2 $coords3" {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set xC [lindex $coords1 end-1]
set yC [lindex $coords1 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Trans. Max" -anchor ne
set xC [lindex $coords2 end-1]
set yC [lindex $coords2 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Trans. Min" -anchor ne
set xC [lindex $coords3 end-1]
set yC [lindex $coords3 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Long." -anchor ne

set graphInfo(title) ""
set graphInfo(xlabel) "Curvature (1/ft)"
set graphInfo(ylabel) "Moment (kip-ft)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Section N-M Interaction -----

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Section Axial-Moment Interaction"; p1 newLine
p1 newLine
p1 setFont 12

global hingeProps

ReportDataLine p1 "Gap Region" ""; p1 newLine
if {$hingeProps(coreConfined) == 1} {
   ReportDataLine p1 "Core Confinement" "Steel Shell"; p1 newLine
} else {
   ReportDataLine p1 "Core Confinement" "Spiral Hoop"; p1 newLine
}
if {$hingeProps(coverConfined) == 1} {
   ReportDataLine p1 "Cover Confinement" "Same as Core"; p1 newLine
} else {
   ReportDataLine p1 "Cover Confinement" "None"; p1 newLine
}
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$w.report.label config -text "Gap Section N-M Interaction"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

set coordsGap [OpenSeesNMAnalysis 2]

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

$w.report.label config -text "CISS Section N-M Interaction"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

set coordsCISS [OpenSeesNMAnalysis 1]


set coordsPile ""
if {$sectionProps(pileSameAsColumn) == 0} {
    $w.report.label config -text "Pile Section N-M Interaction"
    #$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
    update
    
    set coordsPile [OpenSeesNMAnalysis 3]
}

set Nmin 0
set Nmax 0
set Mmax 0

# Figure out clean axis labels
foreach {M N} "$coordsCISS $coordsGap $coordsPile" {
   if {$N < $Nmin} {set Nmin $N}
   if {$N > $Nmax} {set Nmax $N}
   if {$M > $Mmax} {set Mmax $M}
}

axesEMUGraph $graph $Mmax $Nmax 0 $Nmin

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

$graph data d2 -colour blue -points 0 -lines 1 -coords $coordsGap
$graph data d1 -colour red -points 0 -lines 1 -coords $coordsCISS
if {$sectionProps(pileSameAsColumn) == 0} {
    $graph data d3 -colour blue -points 0 -lines 1 -coords $coordsPile
}
$graph redraw

$graph hmark 0.0 tagZero black

if {$sectionProps(pileSameAsColumn) == 1} {
    set xC [lindex $coordsCISS 2]
    set yC [lindex $coordsCISS 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "CISS/Pile Section" -anchor sw
} else {
    set xC [lindex $coordsCISS 2]
    set yC [lindex $coordsCISS 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "CISS Section" -anchor sw

    set xC [lindex $coordsPile 2]
    set yC [lindex $coordsPile 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Pile Section" -anchor sw
}

set xC [lindex $coordsGap 2]
set yC [lindex $coordsGap 3]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Gap Section" -anchor sw

set Mmax 0; set NatMmax 0
if {$sectionProps(pileSameAsColumn) == 1} {
    ReportDataLine p1 "CISS/Pile Section" ""
} else {
    ReportDataLine p1 "CISS Section" ""
}
p1 newLine
foreach {M N} $coordsCISS {
   if {$N == 0} {
      set xC [format %.1f $M]
      set yC [format %.1f $N]
      $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]           -text "($xC,$yC)" -anchor nw
      ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
      p1 newLine
   }
   if {$M > $Mmax} {
      set Mmax $M
      set NatMmax $N
   }
}
set xC [format %.1f $Mmax]
set yC [format %.1f $NatMmax]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]     -text "($xC,$yC)" -anchor w
ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
p1 newLine
p1 newLine


set Mmax 0; set NatMmax 0
if {$sectionProps(pileSameAsColumn) == 0} {
    ReportDataLine p1 "Pile Section" ""
    p1 newLine
    foreach {M N} $coordsPile {
	if {$N == 0} {
	    set xC [format %.1f $M]
	    set yC [format %.1f $N]
	    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  		-text "($xC,$yC)" -anchor nw
	    ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
	    p1 newLine
	}
	if {$M > $Mmax} {
	    set Mmax $M
	    set NatMmax $N
	}
    }
    set xC [format %.1f $Mmax]
    set yC [format %.1f $NatMmax]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "($xC,$yC)" -anchor w
    ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
    p1 newLine
    p1 newLine
}


set Mmax 0; set NatMmax 0
ReportDataLine p1 "Gap Section" ""
p1 newLine
foreach {M N} $coordsGap {
   if {$N == 0} {
      set xC [format %.1f $M]
      set yC [format %.1f $N]
      $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]           -text "($xC,$yC)" -anchor nw
      ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
      p1 newLine
   }
   if {$M > $Mmax} {
      set Mmax $M
      set NatMmax $N
   }
}
set xC [format %.1f $Mmax]
set yC [format %.1f $NatMmax]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]     -text "($xC,$yC)" -anchor w
ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
p1 newLine
p1 newLine


global columnProps
set Ncol $columnProps(N)
for {set i 1} {$i <= $Ncol} {incr i} {
   #$graph hmark $columnProps(axialLoad,$i) tagZero$i red
}

set graphInfo(xlabel) "\nM (kip-ft)"
set graphInfo(ylabel) "N (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Long. R/F Steel -----

$w.report.label config -text "Drawing Long. R/F Steel Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Longitudinal Reinforcing Steel"; p1 newLine
p1 newLine
p1 setFont 12

global rfsteelProps
ReportDataLine p1 "Elastic Modulus, E"               $rfsteelProps(E)      ksi; p1 newLine
ReportDataLine p1 "Yield Stress, fye"                $rfsteelProps(fye)    ksi; p1 newLine
ReportDataLine p1 "Ultimate Stress, fue"             $rfsteelProps(fue)    ksi; p1 newLine
ReportDataLine p1 "Strain-Hardening Onset, epssh"    $rfsteelProps(epssh)  in/in; p1 newLine
ReportDataLine p1 "Strain-Hardening Coefficient, C1" $rfsteelProps(C1)        ; p1 newLine
ReportDataLine p1 "Rupture Strain, epssuR"           $rfsteelProps(epssuR) in/in; p1 newLine
ReportDataLine p1 "Ultimate Strain, epssu"           $rfsteelProps(epssu)  in/in; p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set coords [OpenSeesRebarAnalysis]
$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Jacket Steel -----

$w.report.label config -text "Drawing Steel Shell Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Steel Shell"; p1 newLine
p1 newLine
p1 setFont 12

global jacketProps
ReportDataLine p1 "Elastic Modulus, E"               $jacketProps(E)      ksi; p1 newLine
ReportDataLine p1 "Yield Stress, fye"                $jacketProps(fye)    ksi; p1 newLine
ReportDataLine p1 "Ultimate Stress, fue"             $jacketProps(fue)    ksi; p1 newLine
ReportDataLine p1 "Strain-Hardening Onset, epssh"    $jacketProps(epssh)  in/in; p1 newLine
ReportDataLine p1 "Strain-Hardening Coefficient, C1" $jacketProps(C1)        ; p1 newLine
ReportDataLine p1 "Rupture Strain, epssuR"           $jacketProps(epssuR) in/in; p1 newLine
ReportDataLine p1 "Ultimate Strain, epssu"           $jacketProps(epssu)  in/in; p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set coords [OpenSeesJacketAnalysis]
$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Concrete -----

$w.report.label config -text "Drawing Concrete Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Concrete"; p1 newLine
p1 newLine
p1 setFont 12

global concreteProps
set fc $concreteProps(fc)
set eco $concreteProps(eco)
set Ec [expr {2*$fc/$eco}]
ReportDataLine p1 "Expected Concrete Strength, f\'ce" [format %.2f $fc]  ksi  ; p1 newLine
ReportDataLine p1 "Cracking Strain, eco"              [format %.4f $eco] in/in; p1 newLine
ReportDataLine p1 "Tangent Modulus, Ec"               [format %.2f $Ec]  ksi; p1 newLine
ReportDataLine p1 "Spalling Strain, esp"              [format %.4f $concreteProps(esp)] in/in; p1 newLine
p1 newLine

set fcc $concreteProps(fcc,1)
set ecc $concreteProps(ecc,1)
set fcchoop $concreteProps(fcchoop,1)
set ecchoop $concreteProps(ecchoop,1)

CenterText p1 "Steel Shell (CISS)";
RightText  p1 "Spiral Hoop"; p1 newLine

LeftText   p1 "Compressive Strength, f\'cc"
CenterText p1 "[format %.2f $fcc] ksi"
RightText  p1 "[format %.2f $fcchoop] ksi"; p1 newLine

LeftText   p1 "Peak Strain, ecc"
CenterText p1 "[format %.5f $ecc] in/in"
RightText  p1 "[format %.5f $ecchoop] in/in"; p1 newLine

LeftText   p1 "Secant Modulus, Esec"
CenterText p1 "[format %.2f [expr $fcc/$ecc]] ksi"
RightText  p1 "[format %.2f [expr $fcchoop/$ecchoop]] ksi"; p1 newLine

LeftText   p1 "Mander Coefficient, r"
CenterText p1 "[format %.2f [expr $Ec/($Ec-$fcc/$ecc)]]"
RightText  p1 "[format %.2f [expr $Ec/($Ec-$fcchoop/$ecchoop)]]"; p1 newLine


set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set emax 0
set smax 0

set coords1 [OpenSeesConcreteAnalysis CISS]
$graph data d1 -colour blue -points 0 -lines 1 -coords $coords1
set coords2 [OpenSeesConcreteAnalysis Unconfined]
$graph data d2 -colour red -points 0 -lines 1 -coords $coords2
set coords3 [OpenSeesConcreteAnalysis Hoop]
$graph data d3 -colour black -points 0 -lines 1 -coords $coords3

foreach {e s} "$coords1 $coords2 $coords3" {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw






set xC [lindex $coords1 end-1]
set yC [lindex $coords1 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "CISS" -anchor ne
set xC [lindex $coords2 end-1]
set yC [lindex $coords2 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Unconfined" -anchor sw
set xC [lindex $coords3 end-1]
set yC [lindex $coords3 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC]  	-text "Hoop" -anchor ne


set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 12*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Soil Layers -----

$w.report.label config -text "Drawing Soil p-y Curves"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

global soilProps
set Nlayers $soilProps(Nlayers)

for {set layer 1} {$layer <= $Nlayers} {incr layer} {

   p1 startPage
   WriteHeader p1
   p1 setTextPosition 0 0

   set soilType $soilProps(type,$layer)

   p1 setFont 18 "Times-Roman"
   CenterText p1 "Soil Layer $layer: $soilType"; p1 newLine
   p1 newLine
   p1 setFont 12

   set startDepth 0.0
   for {set i 1} {$i < $layer} {incr i} {
      set startDepth [expr $startDepth + $soilProps(depth,$i)]
   }
   set endDepth [expr {$startDepth + $soilProps(depth,$layer)}]

   ReportDataLine p1 "Depth range" "$startDepth - $endDepth" ft; p1 newLine
   ReportDataLine p1 "p-Multiplier" $soilProps(multiplier,$layer) ""; p1 newLine
   p1 newLine

   if {$soilType == "Sand"} {
      ReportDataLine p1 "Unit weight, gamma"               $soilProps(gamma,$layer) pcf; p1 newLine
      ReportDataLine p1 "Angle of internal friction, phi"  $soilProps(phi,$layer)   deg; p1 newLine
      ReportDataLine p1 "Stiffness increase wrt depth, nh" $soilProps(nh,$layer)    pci; p1 newLine
   }
   if {$soilType == "N.C. Clay"} {
      ReportDataLine p1 "Unit weight, gamma"           $soilProps(gamma,$layer) pcf  ; p1 newLine
      ReportDataLine p1 "Shear strength, c"            $soilProps(c,$layer)     psi  ; p1 newLine
      ReportDataLine p1 "50\% principal strain, eps50" $soilProps(eps50,$layer) in/in; p1 newLine
   }
   if {$soilType == "O.C. Clay"} {
      ReportDataLine p1 "Unit weight, gamma"               $soilProps(gamma,$layer) pcf  ; p1 newLine
      ReportDataLine p1 "Shear strength, c"                $soilProps(c,$layer)     psi  ; p1 newLine
      ReportDataLine p1 "Stiffness increase wrt depth, nh" $soilProps(nh,$layer)    pci  ; p1 newLine
      ReportDataLine p1 "50\% principal strain, eps50"     $soilProps(eps50,$layer) in/in; p1 newLine
   }


   p1 newLine

   set spacing $soilProps(space,$layer)
     
   ReportDataLine p1 "p-y spring spacing"    $spacing ft; p1 newLine

   set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

    set startDepth 0.0
    for {set i 1} {$i < $layer} {incr i} {
	set startDepth [expr $startDepth + $soilProps(depth,$i)]
    }
    set layerDepth $soilProps(depth,$layer)
    set endDepth [expr $startDepth + $layerDepth]

    set numSprings 0

    set NpyLayers [expr int($layerDepth/$spacing)]
    set r [expr 0.5*($layerDepth-$spacing*$NpyLayers)]

    # Do first spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$startDepth + 0.5*$r}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set py($numSprings,depth) $depth
       incr numSprings
    }

    # Do springs in main p-y layes
    for {set i 1} {$i <= $NpyLayers} {incr i} {
       set depth [expr {$startDepth + $r + ($i-0.5)*$spacing}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $spacing]
       set py($numSprings,depth) $depth
       incr numSprings
    }

    # Do last spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$endDepth - 0.5*$r}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set py($numSprings,depth) $depth
       incr numSprings
    }


    set emax 0
    set smax 0
    for {set i 0} {$i < $numSprings} {incr i} {
       foreach {e s} $py($i) {
          if {$e > $emax} {set emax $e}
          if {$s > $smax} {set smax $s}
       }
    }

   ReportDataLine p1 "Number of p-y springs" $numSprings; p1 newLine

   for {set k 0} {$k < $numSprings} {incr k} {
       set color blue
       set depth $py($k,depth)
       if {$depth < $soilProps(waterTableDepth)} {
	   set color black
       }
       $graph data d$k -colour $color -points 0 -lines 1 -coords $py($k)
   }

   axesEMUGraph $graph $emax $smax
   $graph redraw

   for {set k 0} {$k < $numSprings} {incr k} {
       set x [lindex $py($k) end-1]
       set y [lindex $py($k) end]
       set depth [format %.1f $py($k,depth)]
       $gCanvas create text [$graph x2canvas $x] [$graph y2canvas $y] -text "x = $depth ft" -anchor w
   }
    
   set graphInfo(xlabel) "y (ft)"
   set graphInfo(ylabel) "p (kip/ft)"
   labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

   p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

   for {set k 0} {$k < $numSprings} {incr k} {
      $graph clearmark d$k
   }

   destroy $gCanvas

   WritePageNumber p1 [incr pageNumber]
}

#-----

set c [getCanvas]
DefineModel $c

if {0} {
DrawDisplacedShape $c
}

destroy $w

destroy $gCanvas

p1 write -file $openFile
p1 finish
p1 destroy

}

proc createHelp {w menubar label text} {

    upvar $text thisText

    set f [frame $w.help]

    #set t [text $f.t -width 42 -height 4 -wrap word -yscrollcommand "$f.sbar set"]
    #scrollbar $f.sbar -orient vertical -command "$f.t yview"
    #pack $f.sbar -side right -fill y
    #pack $f.t -side left -fill both -expand true

    menubutton $menubar.help -text Help -menu $menubar.help.menu
    pack $menubar.help -side left

    set m [menu $menubar.help.menu]
    $m add command -label "Show/Hide (Ctrl+H)" -command "AboutHelp $f $label"
    bind $w <Control-Key-h> "AboutHelp $w.help $label"

    global toggleHelp
    set toggleHelp($label) 0

    set width [winfo reqwidth $w]
    set Ntext [llength [array names thisText]]
    for {set i 0} {$i < $Ntext} {incr i} {
	message $f.text$i -text $thisText($i) -justify center -width $width
	grid $f.text$i -row $i -column 0	
	#$t insert end "$thisText($i) \n\n"
    }
    return $f
}

proc AboutHelp {f what {side top}} {
    global toggleHelp

    if {$toggleHelp($what) == 0} {
	pack $f -side $side
    } else {
	pack forget $f
    }

    set toggleHelp($what) [expr !$toggleHelp($what)]
}



set propsList    "columnProps    hingeProps    concreteProps    rfsteelProps    jacketProps    sectionProps    soilProps    bentcapProps    limitStates    analysisProps    viewProps"
set propsListTMP "columnPropsTMP hingePropsTMP concretePropsTMP rfsteelPropsTMP jacketPropsTMP sectionPropsTMP soilPropsTMP bentcapPropsTMP limitStatesTMP analysisPropsTMP viewPropsTMP"


proc getCanvas {} {
   return .fBent.canvas
}

proc createTopLevel {w title} {
    toplevel $w
    wm title $w $title
    wm minsize $w 200 100
    focus $w
    #catch {tkwait visibility $w}
    catch {grab $w}
}

proc createOKCancelFrame {w assignProc} {
    pack forget $w.okcancel

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "$assignProc; DefineModel [getCanvas]; destroy $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "destroy $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top    
}

proc createEMUGraph {w name {width 600} {height 600}} {
    pack forget $w.plot
    destroy $w.plot

    frame $w.plot
    canvas $w.plot.canvas -height $height -width $width
    grid $w.plot.canvas -row 0 -column 1;# -sticky nsew
    pack $w.plot -side left

    $w.plot.canvas delete all

    emu_graph::emu_graph $name -canvas $w.plot.canvas -width [expr 0.8*$width] -height [expr 0.9*$height]

    return $w.plot.canvas
}

proc axesEMUGraph {graph xMax yMax {xMin 0} {yMin 0} {nx 5} {ny 5}} {

   $graph configure -autorange 0

   set dx [niceTickMarks $xMin $xMax $nx]
   if {$xMin != 0.0} {
      set xMin [expr $dx*(int($xMin/$dx) - 1)]
   }
   if {$xMax != 0.0} {
      set xMax [expr $dx*(int($xMax/$dx) + 1)]
   }

   set dy [niceTickMarks $yMin $yMax $ny]
   if {$yMin != 0.0} {
      set yMin [expr $dy*(int($yMin/$dy) - 1)]
   }
   if {$yMax != 0.0} {
      set yMax [expr $dy*(int($yMax/$dy) + 1)]
   }

   $graph configure -xmin $xMin
   $graph configure -xmax $xMax
   $graph configure -nticks_x $nx

   $graph configure -ymin $yMin
   $graph configure -ymax $yMax
   $graph configure -nticks_y $ny
}

proc niceTickMarks {xMin xMax nx} {

   # Calculate nice tick marks for axis
   set range [expr double($xMax-$xMin)]
   set tempStep [expr $range/$nx] ;# Initial guess
   set mag [expr int(log10($tempStep))] ;# Magnitude of step size
   set magPow [expr pow(10,$mag)]
   set magMsd [expr int($tempStep/$magPow + 0.5)] ;# Most signif. digit

   # Promote msd to 2, 5, or 10
   if {$magMsd > 5.0} {
      set magMsd 10.0
   } elseif {$magMsd > 2.0} {
      set magMsd 5.0
   } elseif {$magMsd > 1.0} {
      set magMsd 2.0
   } else {
      set magMsd 1.0
   }

   return [expr $magMsd*$magPow]
}

proc labelEMUGraph {c graph graphInfo width height} {

    upvar $graphInfo thisGraphInfo

    $c create text [expr $width/2] 0 -text $thisGraphInfo(title) -anchor n
    $c create text [expr $width/2] $height -text $thisGraphInfo(xlabel) -anchor s
#    $c create text [$graph x2canvas 0] [expr $height-[$graph y2canvas 0]] -text "$thisGraphInfo(ylabel) " -anchor s
    $c create text [$graph x2canvas 0] 0 -text "$thisGraphInfo(ylabel)" -anchor n

    set bbox [$graph bbox]
    $c move all [expr -[lindex $bbox 0]+5] 0.0
}

proc clearCanvas {c {tag all}} {
    foreach id [$c find withtag $tag] {
	$c delete $id 
    }
}

proc DateTime {} {
    #return ""
    set systemTime [clock seconds]

    # This doesn't work on Windows -- MHS
    return "[clock format $systemTime -format %D] [clock format $systemTime -format %H:%M:%S]"
}

proc EngineerName {} {
    global engineerName

    return $engineerName
}

proc BridgeName {} {
    global bridgeName

    return $bridgeName
}

proc PrintCanvas {c w} {
    global PRINTtypelist
    global wd

    set openFile [tk_getSaveFile -defaultextension .ps -filetypes $PRINTtypelist -initialdir $wd -parent $w]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    $c postscript -file $openFile
}

proc ExportData {l w} {
    global DATAtypelist
    global wd

    set openFile [tk_getSaveFile -defaultextension .csv -filetypes $DATAtypelist -initialdir $wd -parent $w]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    set outputStream [open $openFile w]

    foreach {x y} $l {
	puts $outputStream "[format %+.3e $x],[format %+.3e $y]"
    }

    close $outputStream
}


# Create an application menu
##########
frame .mbar -borderwidth 1 -relief raised
pack .mbar -fill x

# The valid file types to display in the file dialog windows
set CISStypelist {
    {"CISS Analysis Files" ".cis"}
    {"CISS Analysis Files" ".CIS"}
    {"All Files" {*} }
}

set PRINTtypelist {
    {"Postscript" ".ps"}
    {"Postscript" ".PS"}
    {"All Files" {*} }
}

set PDFtypelist {
    {"Acrobat" ".pdf"}
    {"Acrobat" ".PDF"}
    {"All Files" {*} }
}

set DATAtypelist {
    {"CSV" ".csv"}
    {"CSV" ".CSV"}
    {"All Files" {*} }
}

set EQtypelist {
    {"CSV" ".csv"}
    {"CSV" ".CSV"}
    {"Plain Text" ".txt"}
    {"Plain Text" ".TXT"}
    {"All Files" {*} }
}

menubutton .mbar.file -text File -menu .mbar.file.menu
pack .mbar.file -side left
set m [menu .mbar.file.menu]
$m add command -label "Open (Ctrl+O)" -command "OpenFile"
bind . <Control-Key-o> {OpenFile}
$m add command -label "Save (Ctrl+S)" -command "SaveFile"
bind . <Control-Key-s> {SaveFile}
$m add command -label "Print (Ctrl+P)" -command "PrintCanvas [getCanvas] .fBent"
bind . <Control-Key-p> {PrintCanvas [getCanvas] .fBent}
$m add command -label "Working Directory (Ctrl+D)" -command "WorkingDir"
bind . <Control-Key-d> {WorkingDir}
$m add separator
$m add command -label "Exit (Ctrl+Q)" -command exit
bind . <Control-Key-q> {exit}

menubutton .mbar.edit -text Edit -menu .mbar.edit.menu
pack .mbar.edit -side left
set m [menu .mbar.edit.menu]
#$m add command -label "Section" -command "DefineSection .section"
$m add cascade -label "Section" -menu $m.sections
set m2 [menu $m.sections]
$m2 add command -label "Column" -command "set whichSection 1; DefineSection .section"
$m2 add command -label "Pile" -command "set whichSection 2; DefineSection .section"

$m add command -label "Gap Region" -command "DefineHinge .hinge"
$m add command -label "Bent Cap" -command "DefineBentcap .bentcap"

$m add cascade -label "Column/Pile Loads" -menu $m.gravity
set m2 [menu $m.gravity]
set N $columnPropsTMP(N)
for {set i 1} {$i <= $N} {incr i} {
    $m2 add command -label "Column/Pile $i" -command "set whichColumn $i; DefineGravityLoad .gravity"
}


$m add cascade -label "Soil" -menu $m.layers
set m2 [menu $m.layers]
$m2 add command -label "Water Table" -command "DefineWaterTable .watertable"
#$m2 add command -label "Frozen Soil" -command "DefineFrozenSoil .frozensoil"
set Nlayers $soilPropsTMP(Nlayers)
for {set i 1} {$i <= $Nlayers} {incr i} {
    $m2 add command -label "Layer $i" -command "set whichLayer $i; DefineSoil .soil"
}

$m add command -label "Earthquake Motion" -command "DefineEQMotion .eqmotion"

$m add separator

$m add command -label "Limit States" -command "DefineLimitStates .limitstates"
#$m add command -label "Analysis Options" -command "DefineAnalysis .analysis"
$m add cascade -label "Analysis Options" -menu $m.analysis
set m2 [menu $m.analysis]
$m2 add command -label "Modeling Options" -command "DefineModelingOptions .modeling"
$m2 add command -label "Pushover Analysis" -command "DefineStaticAnalysis .analysis"
$m2 add command -label "Time History Analysis" -command "DefineDynamicAnalysis .analysis"

$m add separator

$m add cascade -label Color -menu $m.color
set m2 [menu $m.color]
$m2 add command -label Steel -command "ChooseColor colors(steel); DefineModel [getCanvas]"
$m2 add command -label Concrete -command "ChooseColor colors(concrete); DefineModel [getCanvas]"
$m2 add command -label Soil -command "ChooseColor colors(ground); DefineModel [getCanvas]"
$m2 add command -label Pile -command "ChooseColor colors(pile); DefineModel [getCanvas]"
$m2 add separator
$m2 add command -label "Reset Defaults" -command "RestoreDefaultColors; DefineModel [getCanvas]"

proc OpenFile {} {
    global CISStypelist
    global wd

    set openFile [tk_getOpenFile -filetypes $CISStypelist -initialdir $wd]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    global bridgeName
    global engineerName

    global propsList
    foreach props "$propsList colors units" {
	global $props
    }

    global propsListTMP
    foreach props $propsListTMP {
	global $props
    }

    source $openFile

    ChangeSoilLayerMenu .mbar.edit.menu.layers
    ChangeGravityLoadMenu .mbar.edit.menu.gravity

    foreach props $propsList {
	Assign_$props
    }

    #ChangeSoilLayers
    ChangeColumns

    DefineModel [getCanvas]

    ModelIsClean

    SectionIsClean
}

proc SaveFile {} {
    global CISStypelist

    global wd

    set saveFile [tk_getSaveFile -defaultextension .cis -filetypes $CISStypelist -initialdir $wd -initialfile [BridgeName]]

    if {[llength $saveFile] <= 0} {
	return
    }

    set wd [file dirname $saveFile]

    global propsList

    foreach props "$propsList colors units" {
	global $props
    }

    set outFile [open $saveFile w]

    puts $outFile "set bridgeName \"[BridgeName]\""
    puts $outFile "set engineerName \"[EngineerName]\""
    puts $outFile "set versionNumber \"[VersionNumber]\""

    foreach var [array names sectionProps] {
	puts $outFile "set sectionPropsTMP($var) \"$sectionProps($var)\""
    }
    foreach var [lsort [array names soilProps]] {
	puts $outFile "set soilPropsTMP($var) \"$soilProps($var)\""
    }
    foreach var [lsort [array names columnProps]] {
	puts $outFile "set columnPropsTMP($var) \"$columnProps($var)\""
    }
    foreach var [array names hingeProps] {
	puts $outFile "set hingePropsTMP($var) \"$hingeProps($var)\""
    }
    foreach var [array names concreteProps] {
	puts $outFile "set concretePropsTMP($var) \"$concreteProps($var)\""
    }
    foreach var [array names rfsteelProps] {
	puts $outFile "set rfsteelPropsTMP($var) \"$rfsteelProps($var)\""
    }
    foreach var [array names jacketProps] {
	puts $outFile "set jacketPropsTMP($var) \"$jacketProps($var)\""
    }
    foreach var [array names bentcapProps] {
	puts $outFile "set bentcapPropsTMP($var) \"$bentcapProps($var)\""
    }
    foreach var [array names limitStates] {
	puts $outFile "set limitStatesTMP($var) \"$limitStates($var)\""
    }
    foreach var [array names analysisProps] {
	puts $outFile "set analysisPropsTMP($var) \"$analysisProps($var)\""
    }
    foreach var [array names viewProps] {
	puts $outFile "set viewPropsTMP($var) \"$viewProps($var)\""
    }
    foreach var [array names colors] {
	puts $outFile "set colors($var) \"$colors($var)\""
    }
    foreach var [array names units] {
	puts $outFile "set units($var) \"$units($var)\""
    }



#    foreach props $propsList {
#    	foreach var [array names $props] {
#	    puts $outFile "set $props($var) \"$$props($var)\""
#	}
#    }

    close $outFile

    ModelIsClean

    SectionIsClean
}

proc WorkingDir {} {
    global CISStypelist
    global wd

    set dir [tk_chooseDirectory -initialdir $wd]

    if {[llength $dir] <= 0} {
	return
    }

    set wd $dir
}

##########

# Create a canvas to view the bridge bent
##########
frame .fBent
canvas [getCanvas] -height 600 -width 600
grid [getCanvas] -row 0 -column 0
##########

# Create a frame to enter column information
##########
frame .fColInfo -borderwidth 1 -relief raised

label .fColInfo.bname -text "Bridge"
grid .fColInfo.bname -row 0 -column 0 -sticky e
entry .fColInfo.bentry -width 30 -textvariable bridgeName
grid .fColInfo.bentry -row 0 -column 1 -columnspan 3 -sticky w

label .fColInfo.ename -text "Engineer"
grid .fColInfo.ename -row 1 -column 0 -sticky e
entry .fColInfo.eentry -width 30 -textvariable engineerName
grid .fColInfo.eentry -row 1 -column 1 -columnspan 3 -sticky w

label .fColInfo.labelNcol -text "Number of Columns"
grid .fColInfo.labelNcol -row 2 -column 0 -sticky e
entry .fColInfo.entryNcol -width 5 -textvariable columnPropsTMP(N)
bind .fColInfo.entryNcol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryNcol -row 2 -column 1

label .fColInfo.labelHcol -text "Above Grade Height (ft)"
grid .fColInfo.labelHcol -row 3 -column 0 -sticky e
entry .fColInfo.entryHcol -width 5 -textvariable columnPropsTMP(L)
bind .fColInfo.entryHcol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryHcol -row 3 -column 1

label .fColInfo.labelNlay -text "Number of Soil Layers"
grid .fColInfo.labelNlay -row 2 -column 2  -sticky e
entry .fColInfo.entryNlay -width 5 -textvariable soilPropsTMP(Nlayers)
bind .fColInfo.entryNlay <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryNlay -row 2 -column 3

label .fColInfo.labelScol -text "Below Grade Depth (ft)"
grid .fColInfo.labelScol -row 3 -column 2  -sticky e
entry .fColInfo.entryScol -width 5 -textvariable soilPropsTMP(subgradeHeight)
bind .fColInfo.entryScol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryScol -row 3 -column 3

pack .fColInfo -side top

proc CheckMainInput {} {

   global columnPropsTMP

   set N $columnPropsTMP(N)
   if {![isValidInt $N] || $N <= 0} {
	return "number of columns"
   }

   set L $columnPropsTMP(L)
   if {![isValidDouble $L] || $L <= 0} {
	return "above grade height"
   }
   
   global soilPropsTMP

   set Nlayers $soilPropsTMP(Nlayers)
   if {![isValidInt $Nlayers] || $Nlayers <= 0} {
	return "number of soil layers"
   }

   set sgh $soilPropsTMP(subgradeHeight)
   if {![isValidDouble $sgh] || $sgh <= 0.0} {
	return "below grade height"
   }

}

proc DrawBent_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    DefineModel [getCanvas]
}

proc Analyze_Pressed {w {TorL T}} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    #destroy .analysis

    Analyze .analysis $TorL
}

proc Dynamic_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    global eqProps
    if {$eqProps(filename) == "None Selected"} {
	tk_messageBox -type ok -title "Error" -message "No ground motion has been specified" -icon error -parent $w

	DefineEQMotion .eqmotion
	LoadEQMotion .eqmotion
	
	return
    }

    destroy .dynamic

    AnalyzeDynamic .dynamic
}

proc CreateReport_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    CreateReport .fBent.report
}


frame .fReport -borderwidth 1 -relief raised

button .fReport.button -text "Draw Bent" -command "DrawBent_Pressed ."
grid .fReport.button -row 0 -column 0 -columnspan 1 -sticky e
button .fReport.analyze -text "Transverse Pushover" -command "Analyze_Pressed . T"
grid .fReport.analyze -row 0 -column 1 -columnspan 1 -sticky w
button .fReport.analyzeL -text "Longitudinal Pushover" -command "Analyze_Pressed . L"
grid .fReport.analyzeL -row 0 -column 2 -columnspan 1 -sticky w
button .fReport.dynamic -text "Dynamic Analysis" -command "Dynamic_Pressed ."
grid .fReport.dynamic -row 0 -column 3 -columnspan 1 -sticky w
button .fReport.report -text "Create Report" -command "CreateReport_Pressed ."
grid .fReport.report -row 0 -column 4 -columnspan 1 -sticky ew

pack .fReport -side top



menubutton .mbar.help -text Help -menu .mbar.help.menu
pack .mbar.help -side left

set m [menu .mbar.help.menu]
$m add command -label "Show/Hide (Ctrl+H)" -command "DefineModel [getCanvas]; mainHelp .help"
bind . <Control-Key-h> "DefineModel [getCanvas]; mainHelp .help"



set toggleHelp(main) 0
proc mainHelp {w} {
    global toggleHelp
    set toggleHelp(main) [expr !$toggleHelp(main)]

    destroy $w

    if {$toggleHelp(main) == 1} {
	toplevel $w
	wm title $w "Help"
	wm minsize $w 200 100

	frame $w.fHelp
	set width [winfo reqwidth $w]
	
	set text(0) "The \"Bridge Name\" and \"Engineer\" are randomly generated, but can be overwritten, and will be displayed in all data plots."
	set text(1) "Bridge and soil properties can be edited by either double-clicking objects or selecting from the Edit menu."
	set text(2) "\"Draw Bent\" will cause the system to be re-drawn with the current input values."
	set text(3) "\"Transverse Pushover\" will start a transverse pushover analysis and display a plot of the transverse load-displacement response."
	set text(4) "\"Long. Pushover\" will start a longitudinal pushover analysis and display a plot of the longitudinal load-displacement response."
	set text(5) "\"Dynamic Analysis\" will start a transverse time history analysis and display a plot of the transverse displacement time history response."
	set text(6) "\"Create Report\" will create a PDF file summarizing all bent and soil properties and analyses."
	set Ntext [llength [array names text]]
	for {set i 0} {$i < $Ntext} {incr i} {
	    message $w.fHelp.text$i -text $text($i) -justify center -width $width
	    grid $w.fHelp.text$i -row $i -column 0
	    #$helpText insert end "$text($i)\n\n"
	}
	pack $w.fHelp
    }
}

proc DefineModel {c {color on} {TorL T}} {
    global toggleFrame
    global colors

    global units
    foreach var [array names units] {
	set $var $units($var)
    }

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    set lp $hingePropsTMP(lp)

    global bentcapPropsTMP
    set bcap [expr {$bentcapPropsTMP(bcap)*$in}]
    set dcap [expr {$bentcapPropsTMP(dcap)*$in}]

    
    global soilProps
    global soilPropsTMP
    # If number of layers has changed, re-calc depths
    if {$soilProps(Nlayers) != $soilPropsTMP(Nlayers) || 
	$soilProps(subgradeHeight) != $soilPropsTMP(subgradeHeight)} {
	ChangeSoilLayers
	ChangeSoilLayerMenu .mbar.edit.menu.layers
    }

    Assign_soilProps
    
    set subgradeHeight $soilProps(subgradeHeight)
    set waterTableDepth $soilProps(waterTableDepth)
    set frozenSoilDepth $soilProps(frozenSoilDepth)
    set Nlayers $soilProps(Nlayers)
    if {$Nlayers <= 0} {
	return
    }

    
    global columnProps
    global columnPropsTMP
    # If number of columns has changed, assign new values
    if {$columnProps(N) != $columnPropsTMP(N)} {
	ChangeColumns
	ChangeGravityLoadMenu .mbar.edit.menu.gravity
    }
    
    Assign_columnProps
    
    set L $columnProps(L)
    set N $columnProps(N)
    if {$N <= 0} {
	return
    }



    clearCanvas $c

    pack forget $toggleFrame
    pack $toggleFrame -side bottom


    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    set arrowLength [expr {5*$ft}]
    set totalBentHeight [expr {$subgradeHeight + $L + $dcap + $arrowLength}]

    global pixelPerFeet
    set pixelPerFeet(Y) [expr {$height/$totalBentHeight}]

    global sectionPropsTMP
    set Djc [expr {$sectionPropsTMP(Dj,1)*$in}]
    set tjc [expr {$sectionPropsTMP(tj,1)*$in}]
    set Dc [expr {$Djc-2*$tjc}]
    set Dc2 [expr {0.5*$Dc}]
    set Djc2 [expr {0.5*$Djc}]

    set Djp [expr {$sectionPropsTMP(Dj,2)*$in}]
    set tjp [expr {$sectionPropsTMP(tj,2)*$in}]
    set Dp [expr {$Djp-2*$tjp}]
    set Dp2 [expr {0.5*$Dp}]
    set Djp2 [expr {0.5*$Djp}]

    set bentCapOverhang [expr {1.0*$ft}]
    set capoverhangL [expr {$bentcapPropsTMP(leftoverhang)*$in}]
    set capoverhangR [expr {$bentcapPropsTMP(rightoverhang)*$in}]

    set columnWidth 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set columnWidth [expr {$columnWidth + $columnProps(space,$i)}]
    }
    set totalBentWidth [expr {$columnWidth + $Djc + $capoverhangL + $capoverhangR}]
    set viewWidth [expr {$totalBentWidth+0.2*$L}] 
    set pixelPerFeet(X) [expr {$width/$viewWidth}]


    #set x [expr {0.5*($totalBentHeight-$totalBentWidth)}]
    set x [expr {$Djc2 + $capoverhangL + 0.5*0.2*$L}]
    #set x 0
    set x0 $x

    set totalSoilDepth 0.0
    for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	set totalSoilDepth [expr {$totalSoilDepth + $soilProps(depth,$i)}]
    }

    set concreteColor $colors(concrete)
    set steelColor $colors(steel)
    set groundColor $colors(ground)
    set hollowPileColor $colors(pile)
    if {$color != "on"} {
        set concreteColor white
        set steelColor white
        set groundColor white
        set hollowPileColor white
    }

    # Draw columns
    for {set i 1} {$i <= $N} {incr i} {
	set x [expr {$x + $columnProps(space,$i)}]

	if {$i > 1} {
	    $c create text [expr {$x - 0.5*$columnProps(space,$i)}] [expr {$subgradeHeight+0.5*$L}] -text "$columnProps(space,$i) ft o.c." -tag spaceText -anchor s
	}

	set totalHeight [expr {$subgradeHeight+$L}]
	set depthPile $sectionPropsTMP(changePile)

	# Column
	$c create rectangle [expr {$x-0.9*$Dc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+0.9*$Dc2}] $totalHeight -fill $concreteColor -tag column
	if {$tjc > 0.0} {
	    if {$sectionPropsTMP(noConcrete,1) == 0} {
		$c create rectangle [expr {$x-$Djc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+$Djc2}] [expr {$totalHeight-$Gf*$in}] -fill $steelColor -tag column
	    } else {
		$c create rectangle [expr {$x-$Djc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+$Djc2}] [expr {$totalHeight-$Gf*$in}] -fill $hollowPileColor -tag column
	    }
	}
	$c create text $x [expr {$subgradeHeight+0.5*($L-$depthPile)}] -text "Column $i" -anchor s -tag columnText

	# Pile
	if {$tjp > 0.0} {
            if {$sectionPropsTMP(noConcrete,2) == 0} {
		$c create rectangle [expr {$x-$Djp2}] 0 [expr {$x+$Djp2}] [expr {$subgradeHeight-$depthPile}] -fill $steelColor -tag pile
            } else {
		$c create rectangle [expr {$x-$Djp2}] 0 [expr {$x+$Djp2}] [expr {$subgradeHeight-$depthPile}] -fill $hollowPileColor -tag pile
            }
	} else {
	    $c create rectangle [expr {$x-0.9*$Dp2}] 0 [expr {$x+0.9*$Dp2}] [expr {$subgradeHeight-$depthPile}] -fill $concreteColor -tag pile
	}
	set pmT $columnPropsTMP(pMultT,$i)
	set pmL $columnPropsTMP(pMultL,$i)
	$c create text $x [expr {0.5*($subgradeHeight-$depthPile)}] -text "Pile $i\np=$pmT (T)\np=$pmL (L)" -anchor s -tag pileText

	if {$i == 1} {
	    $c create text [expr {$x+$Djc2}] [expr {$subgradeHeight+$L}] -text " Gf = [format %.2f $Gf] in" -anchor nw -tag hinge
	    #$c create text [expr {$x-$Djc2}] [expr {$subgradeHeight+0.5*$L}] -text "[format %.2f $L] ft " -anchor e -tag column
	}

   }

    $c bind spaceText <Double-Button-1> "WhichColumn %x $x0; ColumnSpacing .columnSpacing"
    $c bind hinge <Double-Button-1> {DefineHinge .hinge}
    $c bind column <Double-Button-1> "WhichColumn %x $x0; set whichSection 1; DefineSection .section"
    $c bind pile <Double-Button-1> "WhichColumn %x $x0; set whichSection 2; DefineSection .section"

    # Draw bent cap
    $c create rectangle [expr {$x0-$Djc2-$capoverhangL}] [expr {$totalHeight + $dcap}] [expr {$x0 + $columnWidth+$Djc2+$capoverhangR}] $totalHeight -fill $concreteColor -tag bentcap

    $c bind bentcap <Double-Button-1> {DefineBentcap .bentcap}

    $c create line 0 $subgradeHeight $viewWidth $subgradeHeight -width 4 -tag groundLevel

    set height0 $subgradeHeight
    set xlabel [expr {$x0 + 0.5*$columnProps(space,2)}]
    set xlabel2 [expr {$x0 + $columnProps(space,2) + 0.5*$columnProps(space,3)}]
    for {set i 1} {$i <= $Nlayers} {incr i} {
	set depthi $soilProps(depth,$i)

	set height0 [expr {$height0-$depthi}]

	$c create line 0 $height0 $viewWidth $height0 -width 2
	
	$c create text 0 [expr {$height0 + 0.5*$depthi}] -text "  Layer $i" -anchor w -tag soilText
	$c create text $xlabel [expr {$height0 + 0.5*$depthi}] -text "[format %.2f $soilProps(depth,$i)] ft" -tag soilText
	set pm $soilProps(multiplier,$i)
	set gg $soilProps(gamma,$i)
	set pp $soilProps(phi,$i)
	$c create text $xlabel2 [expr {$height0 + 0.5*$depthi}] -text "$soilProps(type,$i)\np=$pm\ngamma=$gg" -tag soilText
    }

    set waterTableHeight [expr {$subgradeHeight-$waterTableDepth}]
    $c create line 0 $waterTableHeight $viewWidth $waterTableHeight -width 2 -fill blue
    $c create text [expr {2*$x0 + $columnWidth}] $waterTableHeight -text " Water Table, [format %.2f $soilProps(waterTableDepth)] ft" -anchor se -tag water
    $c bind water <Double-Button-1> {DefineWaterTable .watertable}

    set frozenSoilHeight [expr {$subgradeHeight-$frozenSoilDepth}]
    #$c create line 0 $frozenSoilHeight $viewWidth $frozenSoilHeight -width 2 -fill blue
    #$c create text [expr {2*$x0 + $columnWidth}] $frozenSoilHeight -text " Frozen Soil, [format %.2f $soilProps(frozenSoilDepth)] ft" -anchor ne -tag frozensoil
    #$c bind frozensoil <Double-Button-1> {DefineFrozenSoil .frozensoil}

    set x $x0

    set matTag 10

    $c create rectangle 0 0 $viewWidth $subgradeHeight -fill $groundColor -tag soil -width 0
    $c lower soil

    $c bind soil <Double-Button-1> "WhichLayer %y $totalBentHeight; DefineSoil .soil"
    $c bind soilText <Double-Button-1> "WhichLayer %y $totalBentHeight; DefineSoil .soil"





    set x $x0
    # Draw gravity loads
    for {set i 1} {$i <= $N} {incr i} {
	set x [expr {$x + $columnProps(space,$i)}]

	set firstOrLast first
	if {$columnProps(axialLoad,$i) < 0} {
	    set firstOrLast last
	}

	$c create line $x [expr {$totalHeight + $dcap}] $x $totalBentHeight -width 7 -arrow $firstOrLast -tag gravityLoad
      set loadToShow [expr abs($columnProps(axialLoad,$i))]
      set loadToShow [format %.1f $loadToShow]
	$c create text $x $totalBentHeight -text "  $loadToShow\n  kip" -anchor nw -tag gravityLoadText
    }

    $c bind gravityLoad <Double-Button-1> "WhichColumn %x $x0; DefineGravityLoad .gravity"

    foreach obj "gravityLoad hinge column columnText pile pileText spaceText soilText water frozensoil" {
	$c raise $obj
    }
    #$c raise gravityLoad
    #$c raise hinge
    #$c raise column
    #$c raise columnText
    #$c raise spaceText
    #$c raise soilText
    #$c raise water
    #$c raise frozensoil

    if {$pixelPerFeet(X) < $pixelPerFeet(Y)} {
        #set pixelPerFeet(Y) $pixelPerFeet(X)
    } else {
        #set pixelPerFeet(X) $pixelPerFeet(Y)
    }

    $c scale all 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move all 0 $height
    foreach tag "bentcap gravityLoad gravityLoadText column columnText spaceText hinge soilText water" {
	#$c move $tag [expr {0.0*(0.5*$Djc+$bentCapOverhang+0.2*$L)*$pixelPerFeet(X)}] 0
    }
}

set whichColumn 1
proc WhichColumn {x x0} {
    global pixelPerFeet
    set actualX [expr $x/$pixelPerFeet(X)]

    global sectionPropsTMP
    global units

    set Dj2 [expr 0.5*$sectionPropsTMP(Dj,1)*$units(in)]

    global columnProps
    set N $columnProps(N)

    set currentX [expr $x0+$Dj2]

    global whichColumn
    for {set i 1} {$i <= $N} {incr i} {
	if {$actualX <= $currentX} {
	    set whichColumn $i
	    break
	}
	set currentX [expr $currentX + $columnProps(space,[expr $i+1])]
    }
}

set whichLayer 1
proc WhichLayer {y height} {
    global soilProps
    foreach var [array names soilProps] {
	set $var $soilProps($var)
    }

    global pixelPerFeet
    set actualDepth [expr $subgradeHeight - ($height-$y/$pixelPerFeet(Y))]

    global units
    set in $units(in)

    set currentDepth 0.0

    global whichLayer
    for {set i 1} {$i <= $Nlayers} {incr i} {
	set currentDepth [expr $currentDepth + $soilProps(depth,$i)]
	if {$actualDepth <= $currentDepth} {
	    set whichLayer $i
	    break
	}
    }
}

proc checkStrainLimitState {which momentArm whichSections whichElems {TorL T}} {

    global analysisStates
    if {$analysisStates($which,$TorL,disp) > 0} {
	return 0
    }
    
    global limitStatesTMP
    
    set strainLimit $analysisStates($which,val)
    
    if {$analysisStates($which,$TorL,disp) < 0} {
	foreach ele $whichElems {
	    foreach j $whichSections {
		set eps [sectionDeformation $ele $j 1]
		set kap [sectionDeformation $ele $j 2]
		set strainTop [expr {$eps-$kap*$momentArm}]
		set strainBot [expr {$eps+$kap*$momentArm}]
		if {$strainBot >= $strainLimit || $strainTop >= $strainLimit} {
		    return 1
		}
	    }
	}
    }
}

proc SaveNodalDisplacements {{TorL T}} {

    global NodalDisplacements

    set NodalDisplacements(tags,$TorL) ""
    foreach node [getNodeTags] {
       lappend NodalDisplacements(tags,$TorL) $node

       set NodalDisplacements($node,X,$TorL)  [nodeCoord $node X]
       set NodalDisplacements($node,Y,$TorL)  [nodeCoord $node Y]
       set NodalDisplacements($node,UX,$TorL) [nodeDisp $node 1]
       set NodalDisplacements($node,UY,$TorL) [nodeDisp $node 2]
    }
}

proc SaveElementConnectivity {{TorL T}} {

    global ElementConnectivity

    set ElementConnectivity(tags,$TorL) ""
    foreach elem [getEleTags] {
	lappend ElementConnectivity(tags,$TorL) $elem

	set ElementConnectivity($elem,$TorL) [eleNodes $elem]
    }
}

proc SaveMomentDiagram {{TorL T}} {

    global BendingMoments

    global columnProps
    set N $columnProps(N)

    global columnElems
    global pileElems

    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set whichElems $columnElems($i,$TorL)
       if {$SSIon} {
          set whichElems "$whichElems $pileElems($i,$TorL)" ;# pileElems could be empty list
       }

       foreach elem $whichElems {
          set BendingMoments($elem,MI,$TorL) [eleForce $elem 3]
          set BendingMoments($elem,MJ,$TorL) [eleForce $elem 6]
          set BendingMoments($elem,P,$TorL)  [eleForce $elem 5]
          set BendingMoments($elem,VI,$TorL) [eleForce $elem 1]
          set BendingMoments($elem,VJ,$TorL) [eleForce $elem 4]
       }
    }
}

proc OpenSeesGravityAnalysis {{TorL T}} {

    test NormUnbalance 1.0e-3 10 0
    algorithm Newton
    constraints Plain
    numberer RCM
    system UmfPack
    integrator LoadControl 1.0
    analysis Static

    set ok [analyze 1]
    if {$ok < 0} {
	return $ok
    }

    global columnProps
    global columnElems
    global extremeAxialLoad

    global sectionsOfInterest

    # Initialize extreme axial loads
    for {set i 1} {$i <= $columnProps(N)} {incr i} {
       set extremeAxialLoad($i,min,when,$TorL) 0
       set extremeAxialLoad($i,max,when,$TorL) 0

       set ele [lindex $columnElems($i,$TorL) 0]
       set sec [lindex $sectionsOfInterest 0]
       set axialForce [expr -[sectionForce $ele $sec 1]]
       set extremeAxialLoad($i,min,$TorL) $axialForce
       set extremeAxialLoad($i,max,$TorL) $axialForce

       foreach ele $columnElems($i,$TorL) {
          foreach sec $sectionsOfInterest {
             set axialForce [expr -[sectionForce $ele $sec 1]]
             if {$axialForce < $extremeAxialLoad($i,min,$TorL)} {
                set extremeAxialLoad($i,min,$TorL) $axialForce
             }
             if {$axialForce > $extremeAxialLoad($i,max,$TorL)} {
                set extremeAxialLoad($i,max,$TorL) $axialForce
             }
          }
       }
    }

    return 0
}

proc DynamicAnalyzeOneStep {dt} {
    set ok [analyze 1 $dt]
    if {$ok < 0} {
	return $ok
    }
    return 0
}

proc AnalyzeOneStep {{TorL T}} {

   set ok [analyze 1]
   if {$ok < 0} {
      return $ok
   }

   set load [getTime]
   set disp [nodeDisp 3 1]

   global columnElems; global pileElems
   global gapSection; global jacketSection
   set sectionsOfInterest "$gapSection $jacketSection"

   global columnProps
   global extremeAxialLoad
   global bentCapForces

   global analysisStates
   global limitStatesTMP

   global units
   set in $units(in)

   global sectionPropsTMP
   set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
   set tj [expr {$sectionPropsTMP(tj,1)*$in}]
   set Dc [expr {$Dj-2*$tj}]

   set cover [expr {$sectionPropsTMP(cover,1)*$in}]
   set ds $sectionPropsTMP(ds,1)
   set halfBar [expr 0.5*[BarDiam $ds]]

   global rfsteelPropsTMP
   set epsye [expr $rfsteelPropsTMP(fye)/$rfsteelPropsTMP(E)]
   set analysisStates(steelYield,val) $epsye

   set analysisStates(steelLimit,val) $limitStatesTMP(steel)
   set analysisStates(jacketLimit,val) $limitStatesTMP(jacket)
   set analysisStates(concreteLimit,val) $limitStatesTMP(concrete)
   #set analysisStates(concreteLimit,val) [expr -$limitStatesTMP(concrete)]

   global analysisProps

   set N $columnProps(N)

   global bentCapElems
   foreach ele $bentCapElems {
       #puts "$ele: [expr -[eleForce $ele 3]] [eleForce $ele 6]"
   }
   
   # Make sure there is more than one column and analysis is transverse
   if {$N > 1 && $TorL == "T"} {
       # Bent cap forces at inside face of leading pile
       set ele [expr {2*$N+3*$N-1}]
       #puts "Leading element: $ele"
       set bentCapForces(lead,N) [expr [eleForce $ele 4]] ;# tension +ve
       set bentCapForces(lead,V) [expr -[eleForce $ele 5]] ;# down right face +ve
       set bentCapForces(lead,M) [expr [eleForce $ele 6]] ;# top of beam compression +ve

       set q [basicForce $ele]
       set q1 [lindex $q 0]       
       set q2 [lindex $q 1]
       set q3 [lindex $q 2]
       #set bentCapForces(lead,N) [expr $q1]
       #set bentCapForces(lead,M) [expr $q3]
       
       # Bent cap forces at inside face of trailing pile
       set ele [expr {2*$N+5}]
       #puts "Trailing element: $ele"
       set bentCapForces(trail,N) [expr -[eleForce $ele 1]] ;# tension -ve
       set bentCapForces(trail,V) [expr [eleForce $ele 2]] ;# up left face +ve
       set bentCapForces(trail,M) [expr -[eleForce $ele 3]] ;# top of beam compression +ve

       set q [basicForce $ele]
       set q1 [lindex $q 0]       
       set q2 [lindex $q 1]
       set q3 [lindex $q 2]
       #set bentCapForces(trail,N) [expr  $q1]
       #set bentCapForces(trail,M) [expr -$q2]
   }

   for {set i 1} {$i <= $N} {incr i} {
       foreach ele $columnElems($i,$TorL) {
          foreach sec $sectionsOfInterest {
             set axialForce [expr -[sectionForce $ele $sec 1]]
             if {$axialForce < $extremeAxialLoad($i,min,$TorL)} {
                set extremeAxialLoad($i,min,$TorL) $axialForce
                set extremeAxialLoad($i,min,when,$TorL) $load
             }
             if {$axialForce > $extremeAxialLoad($i,max,$TorL)} {
                set extremeAxialLoad($i,max,$TorL) $axialForce
                set extremeAxialLoad($i,max,when,$TorL) $load
             }
          }
       }

      set momentArm [expr {0.5*$Dc-$cover}]
      if {[checkStrainLimitState steelYield $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
         set analysisStates(steelYield,$TorL,disp) $disp
         set analysisStates(steelYield,$TorL,load) $load
      }
      

      set momentArm [expr {0.5*$Dc-$cover-$halfBar}]
      if {[checkStrainLimitState steelLimit $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
         set analysisStates(steelLimit,$TorL,disp) $disp
         set analysisStates(steelLimit,$TorL,load) $load
      }

      set momentArm [expr {0.5*$Dj-$tj}]
      if {$analysisProps(soilStructure) == 1 && [checkStrainLimitState jacketLimit $momentArm $jacketSection $pileElems($i,$TorL) $TorL] > 0} {
         set analysisStates(jacketLimit,$TorL,disp) $disp
         set analysisStates(jacketLimit,$TorL,load) $load
      }

       #set momentArm [expr {0.5*$Dc-$cover}]
       #if {[checkStrainLimitState concreteLimit $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
       #   set analysisStates(concreteLimit,$TorL,disp) $disp
       #   set analysisStates(concreteLimit,$TorL,load) $load
       #}
       
       # Check for user-defined limit strain of concrete
       if {$analysisStates(concreteLimit,$TorL,disp) < 0} {
	   set momentArm [expr {0.5*$Dc-$cover}]
	   foreach ele $columnElems($i,$TorL) {
	       foreach sec $gapSection {
		   set eps [sectionDeformation $ele $sec 1]
		   set kap [sectionDeformation $ele $sec 2]
		   set strainTop [expr {$eps-$kap*$momentArm}]
		   set strainBot [expr {$eps+$kap*$momentArm}]
		   if {$strainBot <= -$limitStatesTMP(concrete) || 
		       $strainTop <= -$limitStatesTMP(concrete)} {
		       set analysisStates(concreteLimit,$TorL,disp) $disp
		       set analysisStates(concreteLimit,$TorL,load) $load
		   }
	       }
	   }
       }
       
   }
   
   return 0
}

proc SaveFinalAxialLoads {{TorL T}} {

    if {$TorL == "L"} {
	return
    }

    global finalAxialLoad
    global columnProps
    global columnElems; global sectionsOfInterest
    
    for {set i 1} {$i <= $columnProps(N)} {incr i} {
	set ele [lindex $columnElems($i,$TorL) 0]
	set sec [lindex $sectionsOfInterest 0]
	set axialForce [expr -[sectionForce $ele $sec 1]]
	set finalAxialLoad($i) $axialForce
    }
}

set OpenSeesPushoverResults(T) ""
set OpenSeesPushoverResults(L) ""
proc OpenSeesPushoverAnalysis {{TorL T}} {

    global OpenSeesPushoverResults

    if {![IsModelDirty] && [llength $OpenSeesPushoverResults($TorL)] > 0} {
       return $OpenSeesPushoverResults($TorL)
    }

    global units
    set in $units(in)
    set ft $units(ft)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    wipe
    
    DefineModel [getCanvas]

    DefineOpenSeesModel $TorL

    set ok [OpenSeesGravityAnalysis $TorL]
    if {$ok < 0} {
       # Do something
    }

    global bentCapElems

    global columnElems
    global columnProps
    set N $columnProps(N)

    global sectionProps
    set Dj $sectionProps(Dj,1)
    global bentcapProps
    set oL $bentcapProps(leftoverhang)
    set oR $bentcapProps(rightoverhang)
    set Lbent [expr $Dj*$in + ($oL+$oR)*$in]
    for {set i 2} {$i <= $N} {incr i} {
	set Lbent [expr {$Lbent + $columnProps(space,$i)}]
    }
    
    global analysisProps

    pattern Plain 2 Linear {
	#load 3 1.0 0.0 0.0
	if {$TorL == "L"} {
	    for {set i 1} {$i <= $N} {incr i} {
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	} else {
	    foreach ele $bentCapElems {
		if {$analysisProps(lateralLoad) == 0} {
		    break
		}
		eleLoad -ele $ele -type beamUniform 0.0 [expr 1.0/$Lbent]
	    }
	    set Nloads [expr 3*$N+2]
	    #load 0 [expr 1.0/$Nloads] 0.0 0.0
	    for {set i 1} {$analysisProps(lateralLoad) == 0 && $i <= $N} {incr i} {
		#load [expr 5*$i-2] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-1] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-0] [expr 1.0/$Nloads] 0.0 0.0
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	    #load [expr 5*$N+1] [expr 1.0/$Nloads] 0.0 0.0
	}
    }

    integrator DisplacementControl 3 1 [expr {0.5*$in}]

    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Static

    # Compute the pushover curve
    set j 0

    global analysisProps
    set maxDisp [expr {$analysisProps(lateralDisp,$TorL)*$ft}]

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    if {$Dj > $maxDisp} {
       #set maxDisp $Dj
    }

    global gapSection
    global jacketSection
    global sectionsOfInterest

    set lastLoad 0.0
    set maxLoad -1.0

    global limitStatesTMP

    global analysisStates

    set analysisStates(steelYield,$TorL,disp) -1.0
    set analysisStates(steelLimit,$TorL,disp) -1.0
    set analysisStates(jacketLimit,$TorL,disp) -1.0
    set analysisStates(concreteLimit,$TorL,disp) -1.0
    set analysisStates(stability,$TorL,disp) -1.0
    set analysisStates(postPeak,$TorL,disp) -1.0
    set analysisStates(maxLoad,$TorL,disp) -1.0

    global analysisProps
    set fractionMaxLoad [expr {$analysisProps(postPeak)/100.0}]

    global stopReport

    SaveElementConnectivity $TorL

    set ok 0
    set disp 0
    set coords "0 0"

    while {$disp < $maxDisp} {

	update
	if {$stopReport == 1} {
	    break
	}
	
	set ok [AnalyzeOneStep $TorL]
	if {$ok < 0} {
	    break
	}
	
	set load [getTime]
	set disp [nodeDisp 3 1]
	lappend coords $disp $load
	
	if {$load > $maxLoad} {
	    set maxLoad $load
	    set analysisStates(maxLoad,$TorL,disp) $disp
	    set analysisStates(maxLoad,$TorL,load) $load
	}
	
	# Check for loss of stability
	if {$analysisStates(stability,$TorL,disp) < 0 && $load < $lastLoad} {
	    set analysisStates(stability,$TorL,disp) $disp
	    set analysisStates(stability,$TorL,load) $load
	}
	set lastLoad $load
	
	# Terminate analysis at specified post-peak capacity
	if {$analysisStates(postPeak,$TorL,disp) < 0 && $load <= [expr {$fractionMaxLoad*$maxLoad}]} {
	    #set stopAnalysis 1
	    set analysisStates(postPeak,$TorL,disp) $disp
	    set analysisStates(postPeak,$TorL,load) $load
	}
	
	
	SaveNodalDisplacements $TorL
	
	SaveMomentDiagram $TorL
	
	incr j
    }
    
    SaveFinalAxialLoads $TorL
    
    set OpenSeesPushoverResults($TorL) $coords
    return $coords
}

set dynamicProgressWindowOpen 0
set dynamicAnalyzeWindowOpen 0
proc AnalyzeDynamic {w} {

    global dynamicProgressWindowOpen
    global dynamicAnalyzeWindowOpen
    if {$dynamicProgressWindowOpen == 1 || $dynamicAnalyzeWindowOpen == 1} {raise $w; return}

    global wd

    global units
    set in $units(in)
    set ft $units(ft)
    set sec $units(sec)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    global stopDynamicAnalysis

    createTopLevel $w "Time History Progress"

    set dynamicProgressWindowOpen 1
    bind $w <Destroy> {set progressWindowOpen 0; set stopDynamicAnalysis 1}

    frame $w.progress

    label $w.progress.label -text "Time" -width 10 
    grid $w.progress.label -row 0 -column 0 -sticky ew
    label $w.progress.update -width 20
    grid $w.progress.update -row 1 -column 0 -sticky ew

    set stopDynamicAnalysis 0
    button $w.progress.cancel -text "Stop" -command "set stopDynamicAnalysis 1"
    grid $w.progress.cancel -row 2 -column 0 -sticky ew

    pack $w.progress -side top

    wipe
    
    DefineModel [getCanvas]

    DefineOpenSeesModel 

    set ok [OpenSeesGravityAnalysis]
    if {$ok < 0} {
       # Do something
    }

    global columnElems

    global eqProps
    set dtGM $eqProps(dtGM)
    set gmFile $eqProps(filename)
    set PGA $eqProps(PGA)

    global analysisProps
    set tFinal $analysisProps(tFinal)

    set g [expr 32.2*$ft/$sec**2]

    if {[file exists $gmFile]} {
	set readGM [open $gmFile r]
	set maxag 0.0
	while {[gets $readGM ag] >= 0} {
	    if {[expr abs($ag)] > $maxag} {
		set maxag [expr abs($ag)]
	    }
	}
	close $readGM
	timeSeries Path 1 -dt $dtGM -filePath $gmFile -factor [expr {($PGA/$maxag)*$g}]
    } else {
	timeSeries Path 1 -time "0 $tFinal" -values "0 0"
    }
    pattern UniformExcitation 2 1 -accel 1

    # Rayleigh damping
    set omega2 [eigen 1]
    set omega [expr sqrt($omega2)]
    set zeta [expr $analysisProps(damping)/100.0] ;# Turn % to decimal
    set aK [expr 2*$zeta/$omega]
    if {$analysisProps(stiffnessProp) == 1} {
	# Proportional to (last committed) tangent stiffness
	rayleigh 0.0 0.0 0.0 $aK
    } else {
	# Proportional to initial stiffness
	rayleigh 0.0 0.0 $aK 0.0
    }

    integrator Newmark 0.5 0.25

    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Transient


    global viewProps

    set dt $analysisProps(timestep)

    global columnProps
    set N $columnProps(N)

    set t 0.0
    set Vbase 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set V [lindex [eleForce [expr 2*$i-1]] 0]
	set Vbase [expr $Vbase + $V]
    }
    set coordsU "0 [nodeDisp 3 1]"
    set coordsV "[nodeDisp 3 1] [expr -$Vbase]"
    while {$stopDynamicAnalysis == 0 && $t < $tFinal} {
	set t [expr {$t+$dt}]

	$w.progress.update config -text "[format %.2f $t] of $tFinal sec"

	update

	set ok [DynamicAnalyzeOneStep $dt]
	if {$ok < 0} {
	    break
	}

	set Vbase 0.0
	for {set i 1} {$i <= $N} {incr i} {
	    set V [lindex [eleForce [expr 2*$i-1]] 0]
	    set Vbase [expr $Vbase + $V]
	}
	lappend coordsU $t [nodeDisp 3 1]
	lappend coordsV [nodeDisp 3 1] [expr -$Vbase]

	SaveNodalDisplacements T
	if {$viewProps(drawD) == 1} {
	    DrawDisplacedShape [getCanvas] T 0
	}
	SaveMomentDiagram T
	if {$viewProps(drawM) == 1} {
	    DrawMomentDiagram [getCanvas] T 0
	}
	if {$viewProps(drawV) == 1} {
	    DrawShearDiagram [getCanvas] T 0
	}
    }

    destroy $w


    global dynamicAnalyzeWindowOpen
    if {$dynamicAnalyzeWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Displacement Time History"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coordsU] $w"
    bind $w <Control-Key-x> "ExportData [list $coordsU] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    foreach {t u} $coordsU {
       if {$t > $emax} {set emax $t}
       if {$u > $smax} {set smax $u}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coordsU

    axesEMUGraph $graph $emax [expr abs($smax)] 0 [expr -abs($smax)]
    $graph redraw

    set graphInfo(title) "Displacement Time History"
    set graphInfo(xlabel) "Time (s)"
    set graphInfo(ylabel) "Displacement (ft)"
    labelEMUGraph $c $graph graphInfo $width $height





    toplevel $w.b
    wm title $w.b "Force-Displacement Response"
    wm minsize $w.b 200 100

    set w $w.b

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coordsV] $w"
    bind $w <Control-Key-x> "ExportData [list $coordsV] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    set emin 0
    set smin 0
    foreach {t u} $coordsV {
       if {$t > $emax} {set emax $t}
       if {$t < $emin} {set emin $t}
       if {$u > $smax} {set smax $u}
       if {$u < $smin} {set smin $u}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coordsV

    axesEMUGraph $graph $emax $smax $emin $smin
    $graph redraw

    set graphInfo(title) "Force-Displacement Response"
    set graphInfo(xlabel) "Displacement (ft)"
    set graphInfo(ylabel) "Base Shear (kip)"
    labelEMUGraph $c $graph graphInfo $width $height
}




set progressWindowOpen 0
set analyzeWindowOpen 0
proc Analyze {w {TorL T}} {

    global progressWindowOpen
    global analyzeWindowOpen
    if {$progressWindowOpen == 1 || $analyzeWindowOpen == 1} {raise $w; return}

    global wd

    global units
    set in $units(in)
    set ft $units(ft)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    global columnElems

    global stopAnalysis

    createTopLevel $w "Analysis Progress"

    set progressWindowOpen 1
    bind $w <Destroy> {set progressWindowOpen 0; set stopAnalysis 1}

    frame $w.progress

    if {$TorL == "T"} {
       label $w.progress.label -text "Transverse Displacement"
    } else {
       label $w.progress.label -text "Longitudinal Displacement"
    }
    grid $w.progress.label -row 0 -column 0 -sticky ew
    label $w.progress.update
    grid $w.progress.update -row 1 -column 0 -sticky ew

    set stopAnalysis 0
    button $w.progress.cancel -text "Stop" -command "set stopAnalysis 1"
    grid $w.progress.cancel -row 2 -column 0 -sticky ew

    pack $w.progress -side top

    
    
    wipe
    
    DefineModel [getCanvas]
    
    DefineOpenSeesModel $TorL
    
    set ok [OpenSeesGravityAnalysis $TorL]
    if {$ok < 0} {
	# Do something
    }
    
    global bentCapElems

    global columnElems
    global columnProps
    set N $columnProps(N)

    global sectionProps
    set Dj $sectionProps(Dj,1)
    global bentcapProps
    set oL $bentcapProps(leftoverhang)
    set oR $bentcapProps(rightoverhang)
    set Lbent [expr $Dj*$in + ($oL+$oR)*$in]
    for {set i 2} {$i <= $N} {incr i} {
	set Lbent [expr {$Lbent + $columnProps(space,$i)}]
    }

    global analysisProps

    pattern Plain 2 Linear {
	#load 3 1.0 0.0 0.0
	if {$TorL == "L"} {
	    for {set i 1} {$i <= $N} {incr i} {
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	} else {
	    foreach ele $bentCapElems {
		if {$analysisProps(lateralLoad) == 0} {
		    break
		}
		eleLoad -ele $ele -type beamUniform 0.0 [expr 1.0/$Lbent]
	    }
	    set Nloads [expr 3*$N+2]
	    #load 0 [expr 1.0/$Nloads] 0.0 0.0
	    for {set i 1} {$analysisProps(lateralLoad) == 0 && $i <= $N} {incr i} {
		#load [expr 5*$i-2] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-1] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-0] [expr 1.0/$Nloads] 0.0 0.0
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	    #load [expr 5*$N+1] [expr 1.0/$Nloads] 0.0 0.0
	}
    }
    integrator DisplacementControl 3 1 [expr {0.5*$in}]
    
    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Static
    
    
    # Compute the pushover curve
    set ok 0
    set j 0
    
    global analysisProps
    set maxDisp [expr {$analysisProps(lateralDisp,$TorL)*$ft}]

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    if {$Dj > $maxDisp} {
	#set maxDisp $Dj
    }
    
    global gapSection
    global jacketSection
    global sectionsOfInterest
    
    set maxLoad 0.0
    set lastLoad 0.0
    set maxLoad -1.0
    
    global analysisStates
    
    set analysisStates(steelYield,$TorL,disp) -1.0
    set analysisStates(steelLimit,$TorL,disp) -1.0
    set analysisStates(jacketLimit,$TorL,disp) -1.0
    set analysisStates(concreteLimit,$TorL,disp) -1.0
    set analysisStates(stability,$TorL,disp) -1.0
    set analysisStates(postPeak,$TorL,disp) -1.0
    set analysisStates(maxLoad,$TorL,disp) -1.0
    
    global analysisProps
    set fractionMaxLoad [expr {$analysisProps(postPeak)/100.0}]
    
    SaveElementConnectivity $TorL
    SaveNodalDisplacements $TorL
    
    #DrawDisplacedShape [getCanvas] 0
    global viewProps

    set disp 0
    set ok 0
    set coords "[nodeDisp 3 1] 0"

    while {$stopAnalysis == 0 && $disp < $maxDisp} {

	$w.progress.update config -text "[format %.2f $disp] / [format %.2f $maxDisp] ft"
	
	update
	
	set ok [AnalyzeOneStep $TorL]
	if {$ok < 0} {
	    break
	}
	
	set load [getTime]
	set disp [nodeDisp 3 1]
	lappend coords $disp $load


	if {$load > $maxLoad} {
	    set maxLoad $load
	    set analysisStates(maxLoad,$TorL,disp) $disp
	    set analysisStates(maxLoad,$TorL,load) $load
	}

	# Check for loss of stability
	if {$analysisStates(stability,$TorL,disp) < 0 && $load < $lastLoad} {
	    set analysisStates(stability,$TorL,disp) $disp
	    set analysisStates(stability,$TorL,load) $load
	}
	set lastLoad $load

	# Terminate analysis at specified post-peak capacity
	if {$analysisStates(postPeak,$TorL,disp) < 0 && $load <= [expr {$fractionMaxLoad*$maxLoad}]} {
	    #set stopAnalysis 1
	    set analysisStates(postPeak,$TorL,disp) $disp
	    set analysisStates(postPeak,$TorL,load) $load
	}

	incr j
	
	SaveNodalDisplacements $TorL
	if {$viewProps(drawD) == 1} {
	    DrawDisplacedShape [getCanvas] $TorL 0
	}
	SaveMomentDiagram $TorL
	if {$viewProps(drawM) == 1} {
	    DrawMomentDiagram [getCanvas] $TorL 0
	}
	if {$viewProps(drawV) == 1} {
	    DrawShearDiagram [getCanvas] $TorL 0
	}

    }

    SaveFinalAxialLoads $TorL

    global OpenSeesPushoverResults
    set OpenSeesPushoverResults($TorL) $coords


    destroy $w

    global analyzeWindowOpen

    if {$analyzeWindowOpen == 1} {raise $w; return}

    if {$TorL == "T"} {
	createTopLevel $w "Transverse Pushover Analysis"
    } else {
	createTopLevel $w "Longitudinal Pushover Analysis"
    }

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    axesEMUGraph $graph $emax $smax
    $graph redraw

    PlotLimitStates $graph $c $TorL

    if {$TorL == "T"} {
	set graphInfo(title) "Transverse Pushover Analysis"
    } else {
	set graphInfo(title) "Longitudinal Pushover Analysis"
    }
    set graphInfo(xlabel) "Displacement (ft)"
    set graphInfo(ylabel) "Load (kip)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

proc PlotLimitStates {graph canvas {TorL T}} {

   global analysisStates

   # Two loops so that text is always on top of lines
   foreach {state} {concreteLimit steelLimit jacketLimit steelYield postPeak} {
      if {$analysisStates($state,$TorL,disp) > 0} {
         $graph vmark $analysisStates($state,$TorL,disp) $state red
      }
   }

   foreach {state label} {concreteLimit "Concrete Limit" steelLimit "Steel Limit" jacketLimit "Shell Limit" steelYield "First Steel Yield" postPeak "Post-Peak"} {
      if {$analysisStates($state,$TorL,disp) > 0} {
         set xc [$graph x2canvas $analysisStates($state,$TorL,disp)]
         set yc [$graph y2canvas $analysisStates($state,$TorL,load)]
	
         $canvas create text $xc $yc -text " $label" -anchor nw
      }
   }

   # Only print maxLoad line if peak has been reached
   if {$analysisStates(maxLoad,$TorL,disp) > 0 && $analysisStates(stability,$TorL,disp) > 0} {
      $graph vmark $analysisStates(maxLoad,$TorL,disp) $state red
      set xc [$graph x2canvas $analysisStates(maxLoad,$TorL,disp)]
      set yc [$graph y2canvas $analysisStates(maxLoad,$TorL,load)]
	
      $canvas create text $xc $yc -text " Peak Capacity" -anchor nw
   }
}

proc DrawDisplacedShape {c {TorL T} {fromStorage 1} {mag 1.0}} {

    clearCanvas $c displacedShape

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]
    
    global units
    
    global soilProps
    set subgradeHeight $soilProps(subgradeHeight)

    set radius [expr {1.0*$units(in)}]
    
    global NodalDisplacements
    if {$fromStorage} {
	set nodeList $NodalDisplacements(tags,$TorL)
    } else {
	set nodeList [getNodeTags]
    }
    foreach node $nodeList {
	if {$fromStorage} {
	    set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	    set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
	    set X [expr {$nodes($node,X) + $mag*$NodalDisplacements($node,UX,$TorL)}]
	    set Y [expr {$nodes($node,Y) + $mag*$NodalDisplacements($node,UY,$TorL)}]
	} else {
	    set nodes($node,X) [nodeCoord $node X]
	    set nodes($node,Y) [nodeCoord $node Y]	
	    set X [expr {$nodes($node,X) + $mag*[nodeDisp $node 1]}]
	    set Y [expr {$nodes($node,Y) + $mag*[nodeDisp $node 2]}]
	}
	$c create oval [expr {$X-$radius}] [expr {$Y-$radius}] [expr {$X+$radius}] [expr {$Y+$radius}] -fill black -tag displacedShape
	#$c create text $X $Y -text " $node" -tag displacedShape
    }
    
    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	set elemList [getEleTags]
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	if {$fromStorage} {
	    set UXI $NodalDisplacements($nodeI,UX,$TorL)
	    set UYI $NodalDisplacements($nodeI,UY,$TorL)
	    set UXJ $NodalDisplacements($nodeJ,UX,$TorL)
	    set UYJ $NodalDisplacements($nodeJ,UY,$TorL)
	} else {
	    set UXI [nodeDisp $nodeI 1]
	    set UYI [nodeDisp $nodeI 2]
	    set UXJ [nodeDisp $nodeJ 1]
	    set UYJ [nodeDisp $nodeJ 2]
	}

	set XI [expr {$nodes($nodeI,X) + $mag*$UXI}]
	set YI [expr {$nodes($nodeI,Y) + $mag*$UYI}]
	set XJ [expr {$nodes($nodeJ,X) + $mag*$UXJ}]
	set YJ [expr {$nodes($nodeJ,Y) + $mag*$UYJ}]
	
	$c create line $XI $YI $XJ $YJ -tag displacedShape
	if {[expr $UXI*$UXJ] < 0} {
	    $c create text $XJ $YJ -text " [format %.1f [expr $YI-$subgradeHeight]] ft" -anchor w -tag displacedShape
	}
    }
    
    global pixelPerFeet
    
    $c scale displacedShape 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move displacedShape 0 $height
}

proc DrawMomentDiagram {c {TorL T} {fromStorage 1} {mag 0.0002}} {

    clearCanvas $c momentDiagram

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    global units

    global BendingMoments

    global NodalDisplacements
    if {$fromStorage} {
       set nodeList $NodalDisplacements(tags,$TorL)
    } else {
       set nodeList [getNodeTags]
    }
    foreach node $nodeList {
       if {$fromStorage} {
	   set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	   set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
       } else {
	   set nodes($node,X) [nodeCoord $node X]
	   set nodes($node,Y) [nodeCoord $node Y]	
       }
    }
        
    set elemList [getEleTags]

    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	set XI [expr $nodes($nodeI,X)]
	set YI [expr $nodes($nodeI,Y)]
	set XJ [expr $nodes($nodeJ,X)]
	set YJ [expr $nodes($nodeJ,Y)]
	
	#$c create line $XI $YI $XJ $YJ -tag momentDiagram
    }
    
    global columnProps
    set N $columnProps(N)

    global soilProps
    set H $soilProps(subgradeHeight)

    set radius [expr {2.0*$units(in)}]

    global columnElems
    global pileElems
    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set maxMPos 0; set maxMPosY 0; set maxMPosX 0
       set maxMNeg 0; set maxMNegY 0; set maxMNegX 0
       set zeroMPosY 0

       foreach elem $columnElems($i,$TorL) {
          if {$fromStorage} {
             set eleNodes $ElementConnectivity($elem,$TorL)
          } else {
             set eleNodes [eleNodes $elem]
          }
          set ndI [lindex $eleNodes 0]
          set ndJ [lindex $eleNodes 1]
	
          set XI [expr $nodes($ndI,X)]
          set YI [expr $nodes($ndI,Y)]
          set XJ [expr $nodes($ndJ,X)]
          set YJ [expr $nodes($ndJ,Y)]

          if {$fromStorage} {
              set MI [expr -$BendingMoments($elem,MI,$TorL)]
              set MJ $BendingMoments($elem,MJ,$TorL)
          } else {
              set MI [expr -[eleForce $elem 3]]
              set MJ [eleForce $elem 6]
          }

          if {$MJ > $maxMPos} {
             set maxMPos $MJ
             set maxMPosY $YJ
          }
          if {$MJ < $maxMNeg} {
             set maxMNeg $MJ
             set maxMNegY $YJ
          }
          if {$MI > $maxMPos} {
             set maxMPos $MI
             set maxMPosY $YI
          }
          if {$MI < $maxMNeg} {
             set maxMNeg $MI
             set maxMNegY $YI
          }

          if {[expr $MI*$MJ] < 0} {
             set YInfl [expr {$YI - $MI*($YJ-$YI)/($MJ-$MI)}]
             $c create oval [expr {$XI-$radius}] [expr {$YInfl-$radius}] [expr {$XI+$radius}] [expr {$YInfl+$radius}] -fill black -tag momentDiagram
             $c create text $XI $YInfl -text " [format %.1f [expr {$YInfl-$H}]] ft" -anchor nw -tag momentDiagram       
          }

          set MI [expr {$MI*$mag}]
          set MJ [expr {$MJ*$mag}]
          $c create line $XI $YI $XJ $YJ -tag momentDiagram
          $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag momentDiagram
       }


       foreach elem $pileElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,MI,$TorL)]
	       set MJ $BendingMoments($elem,MJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 3]]
	       set MJ [eleForce $elem 6]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   # Make sure we are only drawing first occurrence in pile region
	   if {$zeroMPosY == 0 && [expr {$MI*$MJ}] < 0 && $YI < $maxMPosY && $YI < $H} {
	       set zeroMPosY $YI
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag momentDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag momentDiagram
	   #$c create oval [expr {$XI + $MI-$radius}] [expr {$YI-$radius}] [expr {$XI + $MI+$radius}] [expr {$YI+$radius}] -fill black -tag momentDiagram
	   #$c create oval [expr {$XJ + $MJ-$radius}] [expr {$YJ-$radius}] [expr {$XJ + $MJ+$radius}] [expr {$YJ+$radius}] -fill black -tag momentDiagram
       }
       set X [expr {$XI + $maxMPos*$mag}]
       $c create text $X $maxMPosY -text " [format %.1f $maxMPos] kip-ft" -anchor sw -tag momentDiagram
       $c create text $X $maxMPosY -text " [format %.1f [expr {$maxMPosY-$H}]] ft" -anchor nw -tag momentDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMPosY-$radius}] [expr {$X+$radius}] [expr {$maxMPosY+$radius}] -fill black -tag momentDiagram
       
       set X [expr {$XI + $maxMNeg*$mag}]
       $c create text $X $maxMNegY -text " [format %.1f [expr -$maxMNeg]] kip-ft" -anchor sw -tag momentDiagram
       $c create text $X $maxMNegY -text " [format %.1f [expr {$maxMNegY-$H}]] ft" -anchor nw -tag momentDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMNegY-$radius}] [expr {$X+$radius}] [expr {$maxMNegY+$radius}] -fill black -tag momentDiagram
       
       if {$SSIon && $zeroMPosY >= [expr 1.0*$units(ft)]} {
	   #$c create text $XI $zeroMPosY -text " M = 0" -anchor sw -tag momentDiagram
	   $c create text $XI $zeroMPosY -text " [format %.1f [expr {$zeroMPosY-$H}]] ft" -anchor sw -tag momentDiagram       
	   $c create oval [expr {$XI-$radius}] [expr {$zeroMPosY-$radius}] [expr {$XI+$radius}] [expr {$zeroMPosY+$radius}] -fill black -tag momentDiagram
       }
   }
    global pixelPerFeet
    
    $c scale momentDiagram 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move momentDiagram 0 $height
}

proc DrawShearDiagram {c {TorL T} {fromStorage 1} {mag 0.005}} {

    clearCanvas $c shearDiagram

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    global units

    global BendingMoments

    global NodalDisplacements
    if {$fromStorage} {
	set nodeList $NodalDisplacements(tags,$TorL)
    } else {
	set nodeList [getNodeTags]
    }
    foreach node $nodeList {
	if {$fromStorage} {
	    set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	    set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
	} else {
	    set nodes($node,X) [nodeCoord $node X]
	    set nodes($node,Y) [nodeCoord $node Y]	
	}
    }
    
    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	set elemList [getEleTags]
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	set XI [expr $nodes($nodeI,X)]
	set YI [expr $nodes($nodeI,Y)]
	set XJ [expr $nodes($nodeJ,X)]
	set YJ [expr $nodes($nodeJ,Y)]
	
	#$c create line $XI $YI $XJ $YJ -tag shearDiagram
    }

    global columnProps
    set N $columnProps(N)

    global soilProps
    set H $soilProps(subgradeHeight)

    set radius [expr {2.0*$units(in)}]

    global columnElems
    global pileElems
    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set maxMPos 0; set maxMPosY 0; set maxMPosX 0
       set maxMNeg 0; set maxMNegY 0; set maxMNegX 0
       set zeroMPosY 0
       
       foreach elem $columnElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,VI,$TorL)]
	       set MJ $BendingMoments($elem,VJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 1]]
	       set MJ [eleForce $elem 4]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   if {[expr $MI*$MJ] < 0} {
	       set YInfl [expr {$YI - $MI*($YJ-$YI)/($MJ-$MI)}]
	       $c create oval [expr {$XI-$radius}] [expr {$YInfl-$radius}] [expr {$XI+$radius}] [expr {$YInfl+$radius}] -fill black -tag shearDiagram
	       $c create text $XI $YInfl -text " [format %.1f [expr {$YInfl-$H}]] ft" -anchor nw -tag shearDiagram       
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag shearDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag shearDiagram
       }
       
       
       foreach elem $pileElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,VI,$TorL)]
	       set MJ $BendingMoments($elem,VJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 1]]
	       set MJ [eleForce $elem 4]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   # Make sure we are only drawing first occurrence in pile region
	   if {$zeroMPosY == 0 && [expr {$MI*$MJ}] < 0 && $YI < $maxMPosY && $YI < $H} {
	       set zeroMPosY $YI
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag shearDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag shearDiagram
	   #$c create oval [expr {$XI + $MI-$radius}] [expr {$YI-$radius}] [expr {$XI + $MI+$radius}] [expr {$YI+$radius}] -fill black -tag shearDiagram
	   #$c create oval [expr {$XJ + $MJ-$radius}] [expr {$YJ-$radius}] [expr {$XJ + $MJ+$radius}] [expr {$YJ+$radius}] -fill black -tag shearDiagram
       }
       set X [expr {$XI + $maxMPos*$mag}]
       $c create text $X $maxMPosY -text " [format %.1f $maxMPos] kip" -anchor sw -tag shearDiagram
       $c create text $X $maxMPosY -text " [format %.1f [expr {$maxMPosY-$H}]] ft" -anchor nw -tag shearDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMPosY-$radius}] [expr {$X+$radius}] [expr {$maxMPosY+$radius}] -fill black -tag shearDiagram
       
       set X [expr {$XI + $maxMNeg*$mag}]
       $c create text $X $maxMNegY -text " [format %.1f [expr -$maxMNeg]] kip" -anchor sw -tag shearDiagram
       $c create text $X $maxMNegY -text " [format %.1f [expr {$maxMNegY-$H}]] ft" -anchor nw -tag shearDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMNegY-$radius}] [expr {$X+$radius}] [expr {$maxMNegY+$radius}] -fill black -tag shearDiagram
       
       if {$SSIon && $zeroMPosY >= [expr 1.0*$units(ft)]} {
	   #$c create text $XI $zeroMPosY -text " V = 0" -anchor sw -tag shearDiagram
	   $c create text $XI $zeroMPosY -text " [format %.1f [expr {$zeroMPosY-$H}]] ft" -anchor sw -tag shearDiagram       
	   $c create oval [expr {$XI-$radius}] [expr {$zeroMPosY-$radius}] [expr {$XI+$radius}] [expr {$zeroMPosY+$radius}] -fill black -tag shearDiagram
       }
   }
    global pixelPerFeet
    
    $c scale shearDiagram 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move shearDiagram 0 $height
}

# TorL indicates (L)ongitudinal or (T)ransverse model
proc DefineOpenSeesModel {{TorL T}} {

    global units
    foreach var [array names units] {
	set $var $units($var)
    }

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    set lp $hingePropsTMP(lp)

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    set tj [expr {$sectionPropsTMP(tj,1)*$in}]

    global bentcapPropsTMP
    set bcap [expr {$bentcapPropsTMP(bcap)*$in}]
    set dcap [expr {$bentcapPropsTMP(dcap)*$in}]

    
    global soilProps
    global soilPropsTMP
    
    set subgradeHeight $soilProps(subgradeHeight)
    set waterTableDepth $soilProps(waterTableDepth)
    set Nlayers $soilProps(Nlayers)
    if {$Nlayers <= 0} {
	return
    }

    
    global columnProps
    global columnPropsTMP
    
    set L [expr {$columnProps(L)*$ft}]
    set N $columnProps(N)
    if {$N <= 0} {
	return
    }

    set arrowLength [expr {5*$ft}]
    set totalBentHeight [expr {$subgradeHeight + $L + $dcap + $arrowLength}]

    set Dc [expr {$Dj-2*$tj}]
    set Dc2 [expr {0.5*$Dc}]
    set Dj2 [expr {0.5*$Dj}]

    set bentCapOverhang [expr {1.0*$ft}]
    set capoverhangL [expr {$bentcapPropsTMP(leftoverhang)*$in}]
    set capoverhangR [expr {$bentcapPropsTMP(rightoverhang)*$in}]

    set columnWidth 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set columnWidth [expr {$columnWidth + $columnProps(space,$i)}]
    }
    set totalBentWidth [expr {$columnWidth + $Dj2 + $capoverhangL + $capoverhangR}]


    #set x [expr {0.5*($totalBentHeight-$totalBentWidth)}]
    set x [expr {$Dj2 + $capoverhangL + 0.5*0.2*$L}]
    #set x 0
    set x0 $x

    set totalSoilDepth 0.0
    for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	set totalSoilDepth [expr {$totalSoilDepth + $soilProps(depth,$i)}]
    }

    wipe

    model basic -ndm 2 -ndf 3
    
    set linearTransfTag 1
    geomTransf Linear $linearTransfTag
    set corotTransfTag 2
    geomTransf PDelta $corotTransfTag
    
    global analysisProps
    # Turn off large-disp if specified by user
    if {$analysisProps(pDelta) == 0} {
       set corotTransfTag $linearTransfTag
    }

    set CISSSectionTag [CISSSection]
    set GapSectionTag [GapSection]
    set PileSectionTag [PileSection]
    DefineOpenSeesSection $CISSSectionTag $GapSectionTag $PileSectionTag

    set bentcapSectionTag 4
    global concretePropsTMP
    set fc $concretePropsTMP(fc)
    set eco $concretePropsTMP(eco)
    set fc [expr {$fc*$ksi}]
    set Ec [expr {57*sqrt($fc/$psi)*$ksi}]

    section Elastic $bentcapSectionTag [expr 1000*$Ec] [expr {$bcap*$dcap}] [expr {$bcap*pow($dcap,3)/12.0}]

    global gapSection ;# Int.Pt. of gap section to be monitored for strain limit states
    global jacketSection ;# Likewise for jacket section
    global sectionsOfInterest ;# Concatenation of two above

    global columnElems

    set totalHeight [expr {$subgradeHeight+$L}]
	
    if {$TorL == "T"} {
	node 0 [expr $x-$Dj2-$capoverhangL] [expr {$totalHeight+0.5*$dcap}]
    }

    set startElem [expr 2*$N+1]

    global bentCapElems
    set bentCapElems ""

    # Draw columns
    for {set i 1} {$i <= $N} {incr i} {

	set x [expr {$x + $columnProps(space,$i)}]
	
	set ndI [expr {5*$i-4}]
	set ndJ [expr {5*$i-3}]
	set ndK [expr {5*$i-2}]
	
	node $ndI $x $subgradeHeight ;# Ground
	node $ndJ $x $totalHeight ;# Top of column
	if {$TorL == "T"} {
	    node $ndK $x [expr {$totalHeight+0.5*$dcap}] ;# Middle of bent cap
	} else {
	    node $ndK $x [expr {$totalHeight+$dcap}] ;# Top of bent cap
	}
	
	# Turn off SSI if specified to do so by user
	if {$analysisProps(soilStructure) == 0} {
	    fix $ndI 1 1 1
	}
	
	# Column
	#element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag RegularizedHinge Lobatto 5 $CISSSectionTag 0.0 0.0 $jacketlessSectionTag $lp [expr {0.015*$L*$ft}] $jacketSectionTag; set gapSection 5; set jacketSection 1
	element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag HingeRadau $CISSSectionTag $Dc $GapSectionTag $lp $CISSSectionTag; set gapSection 6; set jacketSection 1
	#element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag Lobatto $CISSSectionTag 5; set gapSection 5; set jacketSection 1
	
	# Bent rigid link (vertical)
	element elasticForceBeamColumn [expr {2*$i}] $ndJ $ndK $linearTransfTag Legendre $bentcapSectionTag 2

	#set columnElems($i,$TorL) "[expr {2*$i}] [expr {2*$i-1}]" ;# Includes bent cap
	set columnElems($i,$TorL) "[expr {2*$i-1}]"

	set ndLeft  [expr {5*$i-1}]
	set ndRight [expr {5*$i-0}]
	if {$TorL == "T"} {
	    node $ndLeft  [expr {$x-$Dj2}] [expr {$totalHeight+0.5*$dcap}]
	    node $ndRight [expr {$x+$Dj2}] [expr {$totalHeight+0.5*$dcap}]
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-5] [expr 5*$i-1] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-1] [expr 5*$i-2] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-2] [expr 5*$i-0] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	}
    }

    set sectionsOfInterest "$gapSection $jacketSection"

    # Filler trusses for longitudinal analysis
    for {set i 2} {$i <= $N} {incr i} {
	set ndI [expr {5*($i-1)-2}]
	set ndJ [expr {5*$i-2}]
	if {$TorL == "L"} {
	    incr startElem
	    element truss $startElem $ndI $ndJ $bentcapSectionTag
	}
    }
    
    # Create last node and element for overhang
    if {$TorL == "T"} {
	node [expr 5*$N+1] [expr $x+$Dj2+$capoverhangR] [expr {$totalHeight+0.5*$dcap}]
	incr startElem
	element forceBeamColumn $startElem [expr 5*$N] [expr 5*$N+1] $linearTransfTag Legendre $bentcapSectionTag 2
	lappend bentCapElems $startElem
    }

    # Apply gravity loads
    pattern Plain 1 Constant {
	for {set i 1} {$i <= $N} {incr i} {
	    load [expr {5*$i-2}] 0.0 [expr -abs($columnProps(axialLoad,$i))] 0.0
	}
    }

    # Define mass
    set g [expr 32.2*$ft/$sec**2]
    set massDL $analysisProps(massDL)
    for {set i 1} {$i <= $N} {incr i} {
	set P $columnProps(axialLoad,$i)
	set m [expr $P*$massDL/$g]
	mass [expr {5*$i-2}] $m $m 0.0
    }


    set x $x0

    set matTag 10

    global pileElems

    set startElem [lindex [lsort -real [getEleTags]] end]
    set startNode [lindex [lsort -real [getNodeTags]] end]
    # Loop over column/piles
    for {set i 1} {$i <= $N} {incr i} {

	# p-multiplier for shadowing
	set pmult $columnProps(pMult$TorL,$i)

	set x [expr {$x + $columnProps(space,$i)}]
	
	set pileElems($i,$TorL) " "
	
	# Don't create any soil elements if they're not needed
	if {$analysisProps(soilStructure) == 0} {
	    continue
	}
	
	set ndI [expr {5*$i-4}]
	
	set depthPile $sectionPropsTMP(changePile) ;# depth where section changes from column to pile
	set startDepth 0.0
	for {set j 1} {$j <= $soilProps(Nlayers)} {incr j} {
	    set layerDepth $soilPropsTMP(depth,$j)
	    set endDepth [expr {$startDepth + $layerDepth}]
	    set spacing $soilProps(space,$j)
	    
	    set NpyLayers [expr int($layerDepth/$spacing)]
	    set r [expr {0.5*($layerDepth-$spacing*$NpyLayers)}]
	    
	    set numSprings 0
	    
	    # Do first spring if there is a remainder layer
	    if {$r > 0.0} {
		set depth [expr {$startDepth + 0.5*$r}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}

		OpenSeesSoilSpring $matTag $depth $r $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    # Do springs in main p-y layers
	    for {set k 1} {$k <= $NpyLayers} {incr k} {
		set depth [expr {$startDepth + $r + ($k-0.5)*$spacing}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}

		OpenSeesSoilSpring $matTag $depth $spacing $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    # Do last spring if there is a remainder layer
	    if {$r > 0.0} {
		set depth [expr {$endDepth - 0.5*$r}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}
		
		OpenSeesSoilSpring $matTag $depth $r $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    set startDepth $endDepth
	}
	
	fix $ndJ 0 1 0
    }


    foreach nd [getNodeTags] {
	#puts $nd
    }

    set x $x0
}


DefineModel [getCanvas]

# pdf4tcl license -- reproduced by MHS

# This software is copyrighted by Frank Richter, Jens Pönisch and other 
# parties.  The following terms apply to all files associated with the 
# software unless explicitly disclaimed in individual files.
# 
# The authors hereby grant permission to use, copy, modify, distribute, and 
# license this software and its documentation for any purpose, provided that 
# existing copyright notices are retained in all copies and that this notice 
# is included verbatim in any distributions. No written agreement, license, 
# or royalty fee is required for any of the authorized uses. Modifications 
# to this software may be copyrighted by their authors and need not follow 
# the licensing terms described here, provided that the new terms are 
# clearly indicated on the first page of each file where they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR 
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING 
# OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES 
# THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH 
# DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
# 
# GOVERNMENT USE: If you are acquiring this software on behalf of the U.S. 
# government, the Government shall have only "Restricted Rights" in the 
# software and related documentation as defined in the Federal Acquisition 
# Regulations (FARs) in Clause 52.227.19 (c) (2).  If you are acquiring the 
# software on behalf of the Department of Defense, the software shall be 
# classified as "Commercial Computer Software" and the Government shall have 
# only "Restricted Rights" as defined in Clause 252.227-7013 (c) (1) of 
# DFARs.  Notwithstanding the foregoing, the authors grant the U.S. 
# Government and others acting in its behalf permission to use and 
# distribute the software in accordance with the terms specified in this 
# license.


# snit license -- reproduced by MHS

# This software is copyrighted by William H. Duquette.  The following
# terms apply to all files associated with the software unless
# explicitly disclaimed in individual files.
# 
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
# 
# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 

