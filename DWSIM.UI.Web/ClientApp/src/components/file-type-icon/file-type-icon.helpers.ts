import { IFileTypeIconOptions, getFileTypeIconProps } from "@uifabric/file-type-icons";
import { CustomFileTypeToIconMap, ICustomFileTypeToIconMap } from './file-type-icon.data';
import { IFileTypeIconProps } from './file-type-icon.component';

export interface IFileTypeIconCustomOptions extends IFileTypeIconOptions {
    width: number;
    height: number;
}

export const getFileTypeIconPropsCustom = (options: IFileTypeIconCustomOptions): IFileTypeIconProps | { iconName: string; } => {


    const IconUrl = (getIconImageUrl(options.extension??"") as any).default;
    console.log("IconURl",IconUrl);
    if (IconUrl && IconUrl !== null && IconUrl !== "") {
        return { IconUrl: IconUrl, ...options } as IFileTypeIconProps;
    }
    else {
        return getFileTypeIconProps(options);
    }
}


function getIconImageUrl(extension: string): string {

    if (extension && extension != "") {
        for (const iconName in CustomFileTypeToIconMap) {
            const extensions = CustomFileTypeToIconMap[iconName].extensions;

            if (extensions) {
                for (let i = 0; i < extensions.length; i++) {
                    if (extensions[i].toLowerCase() == extension.toLowerCase()) {
                        return CustomFileTypeToIconMap[iconName].imageUrl;
                    }

                }


            }

        }

    }
    return "";
}


export const getFileExtension = (filename: string) => {
    var filenameArray = filename.split('.');
    if (filenameArray && filenameArray.length > 1) {
        return filenameArray.pop();
    } else {
        return undefined;
    }
}