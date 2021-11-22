import * as React from 'react';
import { Icon, IIconProps } from '@fluentui/react/lib/Icon';
import { IFileTypeIconOptions } from '@uifabric/file-type-icons';

export interface IFileTypeIconProps extends IFileTypeIconOptions {
  iconName?: string;
  IconUrl?: string;
  width?: number;
  height?: number;
  alt?: string;
  className?: string;
  onClick?: () => void
}

export const FileTypeIcon: React.FunctionComponent<IFileTypeIconProps> = (props: IFileTypeIconProps) => {
  
  if (!props.IconUrl) {
    return <Icon {...props}
      onClick={props.onClick} />
  }
  else {
    return <img src={props.IconUrl} className={props.className} {...props}/>
      

  }

}