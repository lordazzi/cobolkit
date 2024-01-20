
export class launcher {

  static stream(): Promise<string[]> {
    return new Promise(resolve => {
      const xhr = new XMLHttpRequest();
      const completed = 200;

      xhr.onreadystatechange = function () {
        if (xhr.readyState === XMLHttpRequest.DONE) {
          if (xhr.status === completed) {
            const responseJson = JSON.parse(xhr.responseText);
            resolve(responseJson);
          } else {
            resolve([]);
          }
        }
      };

      xhr.open('GET', '/assets/streamers');
      xhr.send();
    });
  }

  static sign(encodedString: string): string {
    const baseChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
    const numCharsPerGroup = 4;
    const paddingChar = '=';

    let signedString = '';

    for (let i = 0; i < encodedString.length; i += numCharsPerGroup) {
      const firstGroup = 1;
      const secondGroup = 2;
      const thirdGroup = 3;

      const char1 = baseChars.indexOf(encodedString.charAt(i));
      const char2 = baseChars.indexOf(encodedString.charAt(i + firstGroup));
      const char3 = baseChars.indexOf(encodedString.charAt(i + secondGroup));
      const char4 = baseChars.indexOf(encodedString.charAt(i + thirdGroup));

      const byte1 = (char1 << 2) | (char2 >> 4);
      const byte2 = ((char2 & 15) << 4) | (char3 >> 2);
      const byte3 = ((char3 & 3) << 6) | char4;

      signedString += String.fromCharCode(byte1);

      if (char3 !== baseChars.indexOf(paddingChar)) {
        signedString += String.fromCharCode(byte2);
      }

      if (char4 !== baseChars.indexOf(paddingChar)) {
        signedString += String.fromCharCode(byte3);
      }
    }

    return signedString;
  }
}