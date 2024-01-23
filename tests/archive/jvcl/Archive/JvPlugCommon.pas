{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SxPlugCommon.PAS, released on 2001-11-11.

The Initial Developer of the Original Code is Ralf Steinhaeusser [ralfiii@gmx.net]
Portions created by Ralf Steinhaeusser are Copyright (C) 2001 Ralf Steinhaeusser.
All Rights Reserved.


Contributor(s):

Last Modified: 2002-09-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

 Versionhistory:

 BaseVersion 5 :
 V 11 : When loading packages -> except instead of finally -> //!11
        New event : OnErrorLoading
 V 10 : Now handles custom Plugins (only their destructors are called when unloading)
 V 09 : Pluginmanager : Extension automatically follows plugintype
        First version to share with "rest of the world"
 V 08 : Problems with $ImplicitBuild
 V 07 : fixed file-creation bug: linebreaks were done with #10#13 instead of
              the other way round, what caused the IDE-navigation do show
              erroneous behaviour
 V 06 : fixed Memory leak when loading of not supported DLL's is skipped
        inserted credits to About-box
 V 05 : started adding Package-functionality
        PluginManager : Loined 2 TLists to one,
        Record with info on Plugins introduced
        fixed buggy Instance-count check
        Added PluginKind-Property
        changed : PluginName also contains path
 V 04 : cleaned Plugin-Manager :
        Removed OnBefore- and OnAfterLoading (REALLY unnecessary - OnBeforeLoad,
                and OnAfterLoad are still here !)
        Removed Trigger-routines. Were only called once -> moved into code
 V 03 : removed unnecessary Set/Get-routines for most properties
 V 02 : new about-dialog, removed unnecessary CDK-auto-generated comments
        stupid fPluginHandles from TStringList -> TList
 V 01 : renamed objects, files, ressources
        fixed several Memory-leaks, fixed unload-bug, minimized uses-list
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPlugCommon ;


interface

const C_VersionString = '5.10';
      C_REGISTER_PLUGIN = 'RegisterPlugin';

implementation

end.
