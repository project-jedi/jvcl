unit gcSysRequirements;

{$I glDEF.INC}
{ Constant messages for TglSysRequirements }

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
 