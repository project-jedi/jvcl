{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgConstSysRequirements.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgConstSysRequirements;

{ Constant messages for TJvgSysRequirements }

interface
resourcestring
  {$IFDEF RUSSIAN}
  ERR_VideoVRefreshRate = 'Частота обновления экрана должна быть %d герц или выше. Измените частоту обновления в свойствах экрана.';
  ERR_GraphicResolution = 'Разрешение экрана должно быть %s точек или выше. Измените разрешение в свойствах экрана.';
  ERR_ColorDepth = 'Количество цветов экрана должно быть %s цветов или выше. Измените число цветов в свойствах экрана.';
  ERR_SystemFont = 'В системе должен быть установлен %s шрифт. Измените вид шрифта в свойствах экрана.';
  ERR_OSPlatform = 'Для работы программы необходима операционная система %s.';
  {$ELSE}
  ERR_VideoVRefreshRate = 'Frequency of updating of the screen should be %d hertz or is higher. Change frequency of updating in properties of the screen.';
  ERR_GraphicResolution = 'The sanction of the screen should be equal %s points or is higher. Change the sanction in properties of the screen.';
  ERR_ColorDepth = 'The quantity(amount) of colors of the screen should be equal %s colors or is higher. Change number of colors in properties of the screen.';
  ERR_SystemFont = 'In system the fine font should be established. Change a kind of a font in properties of the screen.';
  ERR_OSPlatform = 'Operational system %s is necessary for work of the program';
  {$ENDIF}
implementation

end.
