{$IFNDEF WINDOWSVERSION_INC}
{$DEFINE WINDOWSVERSION_INC}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: windowsversion.inc, released on 2004-10-28.

You may retrieve the latest version of this file at the JCL home page,
located at http://jcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

// Last modified: $Date$
// For history see end of file

// This file is intended for C header conversions.
// It defines several mutually exclusive IFDEFs which determine
// the Windows version the API conversion is usable with.

// Global switch to make UNICODE versions of API functions default
{.DEFINE UNICODE}

// individual versions including service packs
// ONLY A SINGLE ONE IS ALLOWED TO BE ACTIVATED BY REMOVING THE DOT!
{.$DEFINE WIN95}
{$DEFINE WIN98Gold}
{.$DEFINE WIN98SE}
{.$DEFINE WIN98ME}
{.$DEFINE WINNT35}
{.$DEFINE WINNT351}
{.$DEFINE WINNT4}
{.$DEFINE WINNT4_SP1}
{.$DEFINE WINNT4_SP2}
{.$DEFINE WINNT4_SP3}
{.$DEFINE WINNT4_SP4}
{.$DEFINE WINNT4_SP5}
{.$DEFINE WINNT4_SP6}
{.$DEFINE WIN2000}
{.$DEFINE WIN2000_SP1}
{.$DEFINE WIN2000_SP2}
{.$DEFINE WIN2000_SP3}
{.$DEFINE WIN2000_SP4}
{.$DEFINE WINXP}
{.$DEFINE WINXP_SP1}
{.$DEFINE WINXP_SP2}
{.$DEFINE WIN2003}
{.$DEFINE WINVista}

// secondary IFDEFs for "_UP" which means also any later OS version

{$IFDEF WINVista}    {$DEFINE WINVista_UP}    {$ENDIF}
{$IFDEF WIN2003}     {$DEFINE WIN2003_UP}     {$ENDIF}
{$IFDEF WINXP_SP2}   {$DEFINE WINXP_SP2_UP}   {$ENDIF}
{$IFDEF WINXP_SP1}   {$DEFINE WINXP_SP1_UP}   {$ENDIF}
{$IFDEF WINXP}       {$DEFINE WINXP_UP}       {$ENDIF}
{$IFDEF WIN2000_SP4} {$DEFINE WIN2000_SP4_UP} {$ENDIF}
{$IFDEF WIN2000_SP3} {$DEFINE WIN2000_SP3_UP} {$ENDIF}
{$IFDEF WIN2000_SP2} {$DEFINE WIN2000_SP2_UP} {$ENDIF}
{$IFDEF WIN2000_SP1} {$DEFINE WIN2000_SP1_UP} {$ENDIF}
{$IFDEF WIN2000}     {$DEFINE WIN2000_UP}     {$ENDIF}
{$IFDEF WINNT4_SP6}  {$DEFINE WINNT4_SP6_UP}  {$ENDIF}
{$IFDEF WINNT4_SP5}  {$DEFINE WINNT4_SP5_UP}  {$ENDIF}
{$IFDEF WINNT4_SP4}  {$DEFINE WINNT4_SP4_UP}  {$ENDIF}
{$IFDEF WINNT4_SP3}  {$DEFINE WINNT4_SP3_UP}  {$ENDIF}
{$IFDEF WINNT4_SP2}  {$DEFINE WINNT4_SP2_UP}  {$ENDIF}
{$IFDEF WINNT4_SP1}  {$DEFINE WINNT4_SP1_UP}  {$ENDIF}
{$IFDEF WINNT4}      {$DEFINE WINNT4_UP}      {$ENDIF}
{$IFDEF WINNT351}    {$DEFINE WINNT351_UP}    {$ENDIF}
{$IFDEF WINNT35}     {$DEFINE WINNT35_UP}     {$ENDIF}
{$IFDEF WIN98Gold}   {$DEFINE WIN98Gold_UP}   {$ENDIF}
{$IFDEF WIN98ME}     {$DEFINE WIN98ME_UP}     {$ENDIF}
{$IFDEF WIN98SE}     {$DEFINE WIN98SE_UP}     {$ENDIF}
{$IFDEF WIN98}       {$DEFINE WIN98_UP}       {$ENDIF}
{$IFDEF WIN95}       {$DEFINE WIN95_UP}       {$ENDIF}

// implicit IFDEFs for "_UP" based on initial "_UP" defines

{$IFDEF WINVista_UP}    {$DEFINE WIN2003_UP}     {$ENDIF}
{$IFDEF WIN2003_UP}     {$DEFINE WINXP_SP2_UP}   {$ENDIF}
{$IFDEF WINXP_SP2_UP}   {$DEFINE WINXP_SP1_UP}   {$ENDIF}
{$IFDEF WINXP_SP1_UP}   {$DEFINE WINXP_UP}       {$ENDIF}
{$IFDEF WINXP_UP}       {$DEFINE WIN2000_SP4_UP} {$ENDIF}
{$IFDEF WIN2000_SP4_UP} {$DEFINE WIN2000_SP3_UP} {$ENDIF}
{$IFDEF WIN2000_SP3_UP} {$DEFINE WIN2000_SP2_UP} {$ENDIF}
{$IFDEF WIN2000_SP2_UP} {$DEFINE WIN2000_SP1_UP} {$ENDIF}
{$IFDEF WIN2000_SP1_UP} {$DEFINE WIN2000_UP}     {$ENDIF}
{$IFDEF WIN2000_UP}     {$DEFINE WINNT4_SP6_UP}  {$ENDIF}
{$IFDEF WINNT4_SP6_UP}  {$DEFINE WINNT4_SP5_UP}  {$ENDIF}
{$IFDEF WINNT4_SP5_UP}  {$DEFINE WINNT4_SP4_UP}  {$ENDIF}
{$IFDEF WINNT4_SP4_UP}  {$DEFINE WINNT4_SP3_UP}  {$ENDIF}
{$IFDEF WINNT4_SP3_UP}  {$DEFINE WINNT4_SP2_UP}  {$ENDIF}
{$IFDEF WINNT4_SP2_UP}  {$DEFINE WINNT4_SP1_UP}  {$ENDIF}
{$IFDEF WINNT4_SP1_UP}  {$DEFINE WINNT4_UP}      {$ENDIF}
{$IFDEF WINNT4_UP}      {$DEFINE WINNT351_UP}    {$ENDIF}
{$IFDEF WINNT351_UP}    {$DEFINE WINNT35_UP}     {$ENDIF}
{$IFDEF WINNT35_UP}     {$DEFINE WINNT_UP}       {$ENDIF}
{$IFDEF WINNT_UP}       {$DEFINE WIN98Gold_UP}   {$ENDIF}
{$IFDEF WIN98Gold_UP}   {$DEFINE WIN98ME_UP}     {$ENDIF}
{$IFDEF WIN98ME_UP}     {$DEFINE WIN98SE_UP}     {$ENDIF}
{$IFDEF WIN98SE_UP}     {$DEFINE WIN98_UP}       {$ENDIF}
{$IFDEF WIN98_UP}       {$DEFINE WIN95_UP}       {$ENDIF}

// History:

// $Log$
// Revision 1.3  2006/02/11 16:50:17  marquardt
// another fix to SetupApi.pas, windowsversion.inc defaults to Win98
//
// Revision 1.2  2005/08/28 15:23:33  obones
// Added missing $
//
// Revision 1.1  2004/11/07 20:36:05  marquardt
// Config Manager and Setup API conversions
//

{$ENDIF ~WINDOWSVERSION_INC}
