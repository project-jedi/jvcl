{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgConstSysRequirements.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgConstSysRequirements;

{$I jvcl.inc}

interface

{ Constant messages for TJvgSysRequirements }

{$IFNDEF USEJVCL}
resourcestring
(* RUSSIAN
  RsVideoVRefreshRate = '������� ���������� ������ ������ ���� %d ���� ��� ����. �������� ������� ���������� � ��������� ������.';
  RsGraphicResolution = '���������� ������ ������ ���� %s ����� ��� ����. �������� ���������� � ��������� ������.';
  RsColorDepth = '���������� ������ ������ ������ ���� %s ������ ��� ����. �������� ����� ������ � ��������� ������.';
  RsSystemFont = '� ������� ������ ���� ���������� %s �����. �������� ��� ������ � ��������� ������.';
  RsOSPlatform = '��� ������ ��������� ���������� ������������ ������� %s.';
*)
  RsVideoVRefreshRate = 'The monitor refresh rate should be %d Hertz or higher. Change monitor refresh rate in Monitor Control Panel.';
  RsGraphicResolution = 'The screen resolution should be equal %s pixels or higher. Change screen resolution in Monitor Control Panel.';
  RsColorDepth = 'The number of colors of the screen should be equal to %s colors or higher. Change screen colors in Monitor Control Panel.';
  RsSystemFont = 'In system the small font should be established. Change to small fonts in Monitor Control Panel.';
  RsOSPlatform = 'The program requires %s or better.';
{$ENDIF !USEJVCL}

implementation

end.
