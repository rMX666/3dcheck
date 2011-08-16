unit uConst;

interface

const
  // Errors
  cFilterLoadError = 'Ошибка при загрузке DirectShow фильтра';
  cEnumMediaTypesError = 'Невозможно получить список MediaTypes. Camera, Filter или Graph являются nil (камера %d)';
  cNoMediaTypes = 'Не получено не одного MediaType от IPin (камера %d)';
  cUnableToGetEnumMediaTypes = 'Невозможно получить EnumMediaTypes: IEnumMediaTypes (камера %d)';
  cUnableToGetCaptureVideoPin = 'Невозможно получить Capture-Video-Pin с выбранной камеры (камера %d)';
  cCameraIndexOutOfRange = 'Невозможно выбрать камеру с индексом %d';
  cMediaTypeIndexOutOfRange = 'Невозможно выбрать MediaType с индексом %d';
  cApplyMediaTypeError = 'Не удалось применить выбранный MediaType для камеры %d. Код ошибки: %s';
  cUnableQueryInterface = 'Невозможно запросить интерфейс %s для камеры %d';
  cFilterIsNull = 'Переданный фильтр nil (камера %d)';
  cStartVideoError = 'Ошибка при запуске чтения видео (камера %d). Код ошибки: %s';
  cStartCaptureNotReadyError = 'Приложение не готово с запуску видео';
  cSynchronizeNoneModeError = 'Критическая ошибка синхронизации камер. Невозможно установить источник';
  cSaveWithoutPoints = 'За время проведения испытания не сохранено ни одной точки. Невозможно сохранить испытание.';
  cStartCaptureWithoutFilter = 'Невозможно запустить испытания с выключенным фильтром на камерах';

  cOutOfRangeCoordinate = 'Out of range. %d is not in range [0, %d] OR %d is not in range [0, %d]';
  cOutOfRangeWrappers = 'Out of range. %d is not in range [0, %d]';  

implementation

end.
