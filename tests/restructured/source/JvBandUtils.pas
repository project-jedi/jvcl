{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvBandUtils.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs@ctzen.com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2001-mm-dd

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit jvBandUtils;

interface

function iif(const Test: Boolean; const True, False: Variant): Variant;
function BooleanAsString(const Test: Boolean): string;

implementation

{:Conditional assignment.
Returns the value in True or False depending on the condition Test.
@param  Test    The test condition.
@param  True    Returns this value if Test is True.
@param  False   Returns this value if Test is False.
@return         Value in True or False depending on Test.
@example        <code>
bar := iif(foo, 1, 0);
</code>
<br>has the same effects as:<br>
<code>
if foo then
  bar := 1
else
  bar := 0;
</code>
}

function iif(const Test: Boolean; const True, False: Variant): Variant;
begin
  if Test then
    Result := True
  else
    Result := False;
end;

{:Transforms Boolean to String.
Returns the string 'True' or 'False' depending on Test.
@param  Test    The test condition.
@return         'True' if Test is true, otherwise 'False'.
@example        <code>
ShowMessage('foo is ' + BooleanAsString(foo));
</code>
<br>has the same effects as:<br>
<code>
if foo then
  ShowMessage('foo is True')
else
  ShowMessage('foo is False');
</code>
}

function BooleanAsString(const Test: Boolean): string;
begin
  Result := iif(Test, 'True', 'False');
end;

end.

