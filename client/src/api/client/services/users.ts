import { User, CreateUserRequest } from '../../__generated__/types.gen';
import client from '../client';

export const createUser = (createUserRequest: CreateUserRequest) =>
    client.post('/users', createUserRequest).then((response) => response.data);

export const deleteUser = (userid: string) =>
    client.delete(`/users/${userid}`).then((response) => response.data);

export const getUser = (userid: string) =>
    client.get<User>(`/users/${userid}`).then((response) => response.data);

export const getUsers = async (): Promise<User[]> => {
    const response = await client.get<User[]>('/users');
    return response.data;
  };
