declare global {
  const optimizer: {
    simpleFromString(code: string): string;
    simpleAvailFromString(code: string): string;
    simpleLiveFromString(code: string): string;
    basicBlockFromString(code: string): string;
    ssaFromString(code: string): string;
  };
}

export {};
